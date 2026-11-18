/*
 * Flag filter block.
 *
 * One include-style checkbox per chosen flag column, with the count of rows
 * that box would keep. Ticked boxes union. Unticked contributes NOTHING to
 * the filter, which is the block's whole reason to exist: with Y / N / U / ""
 * all meaning different flavours of "not flagged", a control that only ever
 * adds an inclusion never has to pick which of them the negative means.
 *
 * The gear holds one multi-select over EVERY upstream column (no filtering to
 * flag-shaped ones: that would need reading values, and the picker would go
 * empty whenever the upstream did), and one footnote field per chosen column:
 * the text a `{filters}` caption prints for that flag. Empty uses the label.
 *
 * Depends on: blockr-core.js, blockr-select.js, blockr-blocks.css
 */
(function () {
  'use strict';

  class FlagFilter {
    constructor(el) {
      this.el = el;
      this.columns = [];      // chosen column names, in gear order
      this.selected = {};     // name -> ticked
      this.meta = {};         // name -> {label, count}
      this.matched = null;    // rows kept by the current tick set (server)
      this.total = null;      // rows in the upstream
      this.choices = [];      // every upstream column as {value, label}
      this.footnotes = {};    // name -> footnote text, non-empty only
      this.isDm = false;      // input is a dm: show the Table select
      this.table = null;      // the dm table the flags come from
      this.tables = [];       // dm tables with a flag column
      this._fnFields = {};    // name -> {input, commit}
      this._fnKey = null;     // column set the footnote rows were built for
      this._submitted = false;
      this._callback = null;
      this._popoverOpen = false;
      this._build();
    }

    _build() {
      this.el.innerHTML = '';
      this.card = document.createElement('div');
      this.card.className = 'ffb-card';
      this.el.appendChild(this.card);

      const head = document.createElement('div');
      head.className = 'blockr-gear-header';
      this.gearBtn = document.createElement('button');
      this.gearBtn.type = 'button';
      this.gearBtn.className = 'blockr-gear-btn';
      this.gearBtn.innerHTML = Blockr.icons.gear;
      this.gearBtn.title = 'Choose flag columns';
      this.gearBtn.addEventListener('click', (e) => {
        e.stopPropagation();
        this._togglePopover();
      });
      head.appendChild(this.gearBtn);
      this.card.appendChild(head);

      // In-flow settings band, not a floating popover: opening it pushes the
      // body down, so it can never overflow a narrow dock panel.
      this.band = document.createElement('div');
      this.band.className = 'ffb-settings';
      this.band.style.display = 'none';

      // Table select, shown only when the input is a dm.
      this.tableRow = document.createElement('div');
      this.tableRow.className = 'blockr-popover-row ffb-table-row';
      this.tableRow.style.display = 'none';
      const tlab = document.createElement('label');
      tlab.className = 'blockr-popover-label';
      tlab.textContent = 'Table';
      this.tableRow.appendChild(tlab);
      const tcol = document.createElement('div');
      tcol.className = 'ffb-field';
      this.tableWrap = document.createElement('div');
      this.tableWrap.className = 'blockr-popover-select-wrap';
      tcol.appendChild(this.tableWrap);
      this.tableHint = document.createElement('div');
      this.tableHint.className = 'ffb-hint';
      tcol.appendChild(this.tableHint);
      this.tableRow.appendChild(tcol);
      this.band.appendChild(this.tableRow);

      const row = document.createElement('div');
      row.className = 'blockr-popover-row';
      const lab = document.createElement('label');
      lab.className = 'blockr-popover-label';
      lab.textContent = 'Flags';
      row.appendChild(lab);
      const pcol = document.createElement('div');
      pcol.className = 'ffb-field';
      this.pickWrap = document.createElement('div');
      this.pickWrap.className = 'blockr-popover-select-wrap';
      pcol.appendChild(this.pickWrap);
      this.pickHint = document.createElement('div');
      this.pickHint.className = 'ffb-hint';
      this.pickHint.style.display = 'none';
      pcol.appendChild(this.pickHint);
      row.appendChild(pcol);
      this.band.appendChild(row);

      this.notesRow = document.createElement('div');
      this.notesRow.className = 'blockr-popover-row ffb-notes-row';
      const nlab = document.createElement('label');
      nlab.className = 'blockr-popover-label';
      nlab.textContent = 'Footnote';
      this.notesRow.appendChild(nlab);
      this.notesWrap = document.createElement('div');
      this.notesWrap.className = 'ffb-notes';
      this.notesRow.appendChild(this.notesWrap);
      this.band.appendChild(this.notesRow);
      this.card.appendChild(this.band);

      this.body = document.createElement('div');
      this.body.className = 'ffb-body';
      this.card.appendChild(this.body);

      this.readout = document.createElement('div');
      this.readout.className = 'ffb-readout';
      this.card.appendChild(this.readout);
    }

    _togglePopover() {
      this._popoverOpen = !this._popoverOpen;
      this.band.style.display = this._popoverOpen ? 'block' : 'none';
      this.gearBtn.classList.toggle('blockr-gear-active', this._popoverOpen);
      Object.values(this._fnFields).forEach((f) => this._fitNote(f.input));
    }

    // Picking a table drops the ticks, which belong to the old table. The
    // server answers with the new table's flags.
    _rebuildTableSelect() {
      this.tableRow.style.display = this.isDm ? '' : 'none';
      this.pickHint.style.display = (this.isDm && this.table) ? '' : 'none';
      this.pickHint.textContent = this.table ? 'Columns of ' + this.table : '';
      if (!this.isDm) return;
      this.tableHint.textContent = this.tables.length
        ? 'Tables with flag columns: ' + this.tables.join(', ')
        : 'No table of this dm has a Y/N flag column';
      this.tableWrap.innerHTML = '';
      Blockr.Select.single(this.tableWrap, {
        options: this.tables.map((t) => ({ value: t, label: '' })),
        selected: this.table || '',
        placeholder: 'Select table…',
        onChange: (t) => {
          if (!t || t === this.table) return;
          this.table = String(t);
          this.selected = {};
          this.matched = null;
          this.pickHint.textContent = 'Columns of ' + this.table;
          this._renderReadout();
          this._submit();
        }
      });
    }

    _rebuildPicker() {
      this.pickWrap.innerHTML = '';
      Blockr.Select.multi(this.pickWrap, {
        options: this.choices.slice(),
        selected: this.columns.slice(),
        placeholder: 'Select columns…',
        reorderable: false,
        onChange: (vals) => {
          const next = vals.map(String);
          // A column dropped from the gear loses its tick with it.
          const sel = {};
          next.forEach((n) => { if (this.selected[n]) sel[n] = true; });
          this.columns = next;
          this.selected = sel;
          this.matched = null;
          this._renderBody();
          this._renderFootnotes();
          this._submit();
        }
      });
    }

    _renderBody() {
      this.body.innerHTML = '';
      if (this.columns.length === 0) {
        const empty = document.createElement('div');
        empty.className = 'ffb-empty';
        empty.textContent = 'No flags. Click the gear to choose columns.';
        this.body.appendChild(empty);
        this._renderReadout();
        return;
      }

      const maxCount = this.columns.reduce((m, n) => {
        const c = (this.meta[n] || {}).count;
        return (typeof c === 'number' && c > m) ? c : m;
      }, 0);

      this.columns.forEach((name) => {
        const m = this.meta[name] || {};
        const rowEl = document.createElement('label');
        rowEl.className = 'ffb-row';

        // The tick, as Blockr.Select's multi list draws it: in a slot every
        // row keeps, so the names stay in line. The input stays for the
        // keyboard and screen readers.
        const input = document.createElement('input');
        input.type = 'checkbox';
        input.className = 'ffb-input';
        input.checked = !!this.selected[name];
        const box = document.createElement('span');
        box.className = 'ffb-tick';
        box.setAttribute('aria-hidden', 'true');
        const drawTick = () => {
          box.innerHTML = input.checked ? Blockr.icons.confirm : '';
          rowEl.classList.toggle('ffb-row--on', input.checked);
          this.body.classList.toggle('ffb-body--picked',
            Object.keys(this.selected).length > 0);
        };
        input.addEventListener('change', () => {
          if (input.checked) this.selected[name] = true;
          else delete this.selected[name];
          drawTick();
          this.matched = null;   // stale until the server recounts
          this._renderReadout();
          this._submit();
        });

        // The crossfilter card's row: the name and its label on one line,
        // ending in "…" (the tooltip has both in full), the bar and the count
        // under them.
        const text = document.createElement('span');
        text.className = 'ffb-text';
        const nm = document.createElement('span');
        nm.className = 'ffb-name';
        nm.textContent = name;
        text.appendChild(nm);
        if (m.label) {
          const lb = document.createElement('span');
          lb.className = 'ffb-label';
          lb.textContent = m.label;
          text.appendChild(lb);
        }
        rowEl.title = m.label ? name + ': ' + m.label : name;

        rowEl.appendChild(input);
        rowEl.appendChild(box);
        rowEl.appendChild(text);

        if (typeof m.count === 'number') {
          const line = document.createElement('span');
          line.className = 'ffb-bar-line';
          const bar = document.createElement('span');
          bar.className = 'ffb-bar';
          const fill = document.createElement('i');
          fill.style.width = maxCount > 0
            ? (m.count / maxCount) * 100 + '%'
            : '0';
          bar.appendChild(fill);
          line.appendChild(bar);

          const ct = document.createElement('span');
          ct.className = 'ffb-count';
          ct.textContent = String(m.count);
          line.appendChild(ct);
          rowEl.appendChild(line);
        }
        drawTick();

        this.body.appendChild(rowEl);
      });

      this._renderReadout();
    }

    // One commit field per chosen column. Rebuilt only when the column set
    // changes: a metadata push that merely brings labels updates the
    // placeholders in place, so a field being typed in keeps its cursor.
    _renderFootnotes() {
      const key = this.columns.join('\u0001');
      if (key !== this._fnKey) {
        this._fnKey = key;
        this._fnFields = {};
        this.notesWrap.innerHTML = '';
        this.columns.forEach((name) => {
          const line = document.createElement('div');
          line.className = 'ffb-note';
          const nm = document.createElement('span');
          nm.className = 'ffb-note-name';
          nm.textContent = name;
          line.appendChild(nm);
          const field = document.createElement('div');
          field.className = 'blockr-commit-field';
          // A textarea, because the text is a sentence and the panel is
          // narrow. textCommit takes Enter as the commit, so it never holds
          // a line break.
          const input = document.createElement('textarea');
          input.rows = 1;
          input.className = 'blockr-text-input ffb-note-input';
          input.value = this.footnotes[name] || '';
          input.addEventListener('input', () => this._fitNote(input));
          field.appendChild(input);
          const commit = Blockr.textCommit(input, {
            onCommit: (value) => {
              const v = value.trim();
              if (v) this.footnotes[name] = v;
              else delete this.footnotes[name];
              this._submit();
            }
          });
          line.appendChild(field);
          this.notesWrap.appendChild(line);
          this._fnFields[name] = { input, commit };
        });
      } else {
        this.columns.forEach((name) => {
          const f = this._fnFields[name];
          if (f && document.activeElement !== f.input) {
            f.commit.sync(this.footnotes[name] || '');
          }
        });
      }
      this.notesRow.style.display = this.columns.length ? '' : 'none';
      this.columns.forEach((name) => {
        const f = this._fnFields[name];
        if (f) {
          f.input.placeholder = (this.meta[name] || {}).label || name;
          this._fitNote(f.input);
        }
      });
    }

    // Grow the field to its text. A closed band has no layout, so this runs
    // again when the gear opens.
    _fitNote(input) {
      if (!input.offsetParent) return;
      input.style.height = 'auto';
      input.style.height = input.scrollHeight + 2 + 'px';
    }

    // The readout is what makes "unticked means everything" self-evident:
    // clearing the boxes visibly returns the row count to the total.
    //
    // `matched` is computed server-side, because summing the per-flag counts
    // here would double-count a row carrying two flags and could print more
    // rows than the table has. Until it arrives, say nothing rather than
    // guess.
    //
    // In dm mode the readout names the table, so it is clear the rest of the
    // dm passes through.
    _renderReadout() {
      const ticked = this.columns.filter((n) => this.selected[n]);
      const total = this.total;
      let text;
      if (ticked.length === 0) {
        text = total == null
          ? 'No filter'
          : 'No filter · ' + total + ' of ' + total + ' rows';
      } else if (this.matched == null) {
        text = ticked.length + ' of ' + this.columns.length +
          ' flags · counting…';
      } else {
        text = total == null
          ? this.matched + ' rows'
          : this.matched + ' of ' + total + ' rows';
      }
      this.readout.innerHTML = '';
      if (this.isDm && this.table) {
        const b = document.createElement('b');
        b.textContent = this.table;
        this.readout.appendChild(b);
        this.readout.appendChild(document.createTextNode(': '));
        text += ' · other tables unchanged';
      }
      this.readout.appendChild(document.createTextNode(text));
    }

    updateCounts(payload) {
      this.matched = (payload && payload.matched != null)
        ? payload.matched : null;
      if (payload && payload.total != null) this.total = payload.total;
      this._renderReadout();
    }

    updateMeta(payload) {
      const cols = (payload && payload.columns) || [];
      const meta = {};
      const names = cols.map((c) => {
        meta[c.name] = {
          label: c.label || '',
          count: (c.count === null || c.count === undefined) ? undefined : c.count
        };
        if (c.total !== null && c.total !== undefined) this.total = c.total;
        return c.name;
      });
      this.meta = meta;
      // `tables` is null for a data frame, a list (maybe empty) for a dm.
      this.isDm = Array.isArray(payload && payload.tables);
      this.tables = this.isDm ? payload.tables.map(String) : [];
      this.table = (this.isDm && payload.table != null)
        ? String(payload.table) : null;
      // Labels and counts for a column set the client picked itself. Touching
      // the picker or the ticks here would fight the click that caused it.
      if (payload && payload.meta_only) {
        this._renderBody();
        this._renderFootnotes();
        return;
      }
      this.columns = names;
      this._rebuildTableSelect();
      // {value, label} options, so the picker prints the ADaM label beside
      // the column name. A bare string is still accepted.
      this.choices = ((payload && payload.choices) || []).map((c) =>
        (c && typeof c === 'object')
          ? { value: String(c.value), label: c.label || '' }
          : { value: String(c), label: '' });
      const sel = {};
      ((payload && payload.selected) || []).forEach((n) => { sel[String(n)] = true; });
      this.selected = sel;
      // An empty R list arrives as [], a named one as an object.
      const notes = {};
      const fn = payload && payload.footnotes;
      if (fn && !Array.isArray(fn) && typeof fn === 'object') {
        Object.keys(fn).forEach((k) => { if (fn[k]) notes[k] = String(fn[k]); });
      }
      this.footnotes = notes;
      this._rebuildPicker();
      this._renderBody();
      this._renderFootnotes();
    }

    _compose() {
      const footnotes = {};
      this.columns.forEach((n) => {
        if (this.footnotes[n]) footnotes[n] = this.footnotes[n];
      });
      return {
        columns: this.columns.slice(),
        selected: this.columns.filter((n) => this.selected[n]),
        footnotes: footnotes,
        table: this.isDm ? this.table : null
      };
    }

    _submit() {
      this._submitted = true;
      if (this._callback) this._callback(true);
    }

    getValue() {
      return this._submitted ? this._compose() : null;
    }

    subscribe(cb) { this._callback = cb; }
    unsubscribe() { this._callback = null; }
  }

  const binding = new Shiny.InputBinding();
  Object.assign(binding, {
    find: (scope) => $(scope).find('.ffb-container'),
    getId: (el) => el.id || null,
    initialize: (el) => {
      if (!el._block) el._block = new FlagFilter(el);
      // Announce, so the server re-pushes metadata. On a deferred dock panel
      // this script arrives WITH the panel, and anything sent earlier was
      // dropped by Shiny for want of a handler.
      if (window.Shiny && Shiny.setInputValue) {
        Shiny.setInputValue(el.id + '_ready', Date.now(), { priority: 'event' });
      }
    },
    getValue: (el) => el._block ? el._block.getValue() : null,
    subscribe: (el, cb) => { if (el._block) el._block.subscribe(cb); },
    unsubscribe: (el) => { if (el._block) el._block.unsubscribe(); }
  });
  Shiny.inputBindings.register(binding, 'blockr.pharma.flagFilter');

  Shiny.addCustomMessageHandler('pharma-flag-meta', (msg) => {
    const el = document.getElementById(msg.id);
    if (el && el._block) el._block.updateMeta(msg);
  });

  Shiny.addCustomMessageHandler('pharma-flag-counts', (msg) => {
    const el = document.getElementById(msg.id);
    if (el && el._block) el._block.updateCounts(msg);
  });
})();
