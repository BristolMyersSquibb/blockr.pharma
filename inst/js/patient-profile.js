/* Patient profile block: the client half.
 *
 * Everything the block does between server round trips lives here: the
 * cohort list (keyboard walk, pick debounce, band drawing, sizing), the
 * panel picker (search, first hit, the move animation), the panels (ghost
 * during a patient switch, drag reorder, chart resize) and the header
 * controls. R renders the markup and sends six custom messages; this file
 * owns the DOM after that.
 *
 * Mounted once per block instance from the block's UI:
 *
 *   PatientProfile.mount({id: "<shiny namespace>", grip: "<svg>", bandH: 8})
 *
 * Every element and input id derives from `id` the way shiny::NS() builds
 * them (`id + "-" + name`), so the file carries no id of its own. `grip` is
 * the drag-handle glyph the sidebar rows reuse, `bandH` the height of a
 * spans band (pp_cohort_band_h_spans in R).
 *
 * Extracted verbatim from the inline script in R/patient-profile-block.R
 * (0.0.0.9050) so the browser tests in tests/testthat/test-shinytest2.R
 * can vouch for the move; the refactor comes after.
 */
(function() {
  window.PatientProfile = {
    mount: function(cfg) {
      var ns = function(name) { return cfg.id + '-' + name; };
      var layoutId = ns('pp_layout');
      var sidebarId = ns('pp_sidebar');
      var searchId = ns('search');
      var toggleInputId = ns('toggle_viz');
      var clearBtnId = ns('search_clear');
      var ctrlInputId = ns('viz_ctrl');
      var pickParamInputId = ns('pick_param');
      var pickSubjectInputId = ns('pick_subject');
      var cohortSortInputId = ns('cohort_sort');
      var cohortSortById = ns('cohort_sort_by');
      var cohortWellId = ns('pp_cohort_well');
      var chartAreaId = ns('chart_area');
      var syncBandMsgId = ns('sync_band');
      var addOnId = ns('pp_add_on');
      var GRIP_SVG = cfg.grip;
      var syncSubjectMsgId = ns('sync_subject');
      var syncMsgId = ns('sync_selected');
      var syncParamsMsgId = ns('sync_params');
      var reorderInputId = ns('reorder_viz');
      var tlModeInputId = ns('timeline_mode');
      var prestudyInputId = ns('show_prestudy');
      var smoothInputId = ns('smooth_mode');
      var gearBtnId = ns('pp_gear_btn');
      var gearPopoverId = ns('pp_gear_popover');

      var pickerId = ns('pp_picker');
      var cohortCountId = ns('pp_cohort_count');
      var subjectPickerMsgId = ns('subject_picker');
      var dlMenuMsgId = ns('dl_menu_state');
      var dlRootId = ns('pp_dl_root');
      var dlLabelPatientId = ns('pp_dl_label_patient');
      var dlLabelCohortId = ns('pp_dl_label_cohort');

      // The cohort tag: its number, and whether it shows at all.
      //
      // What is left of the subject_picker message. It used to mount a
      // Blockr.Select over every patient and keep its options in step;
      // the sidebar's cohort list does that job, so the message now
      // carries the count and nothing else. Cohort-scoped by
      // construction: this handler only runs when the cohort changes,
      // never on a patient switch.
      Shiny.addCustomMessageHandler(subjectPickerMsgId, function(msg) {
        if (!msg) return;
        var n = msg.count || 0;
        var $count = $('#' + cohortCountId);
        $count.find('.pp-cohort-count-n').text(
          n.toLocaleString() + (n === 1 ? ' patient' : ' patients'));
        $count.toggleClass('is-hidden', !n);
      });

      // Download-menu scope sync. The menu is rendered ONCE (see
      // header_bar); this updates its two section labels and their
      // visibility so a patient switch never re-renders -- and never
      // blinks -- the button. The <details> stays as-is: text
      // updates do not close an open menu.
      function applyDlMenu(msg, tries) {
        var $root = $('#' + dlRootId);
        if (!$root.length) {
          // The first state message can beat the header render (the
          // menu mounts with it); retry briefly rather than dropping
          // the state on the floor.
          if (tries > 0) {
            setTimeout(function() { applyDlMenu(msg, tries - 1); },
                       100);
          }
          return;
        }
        var single = !!msg.single;
        var n = msg.n || 0;
        $('#' + dlLabelPatientId).text(
          single ? 'This patient: ' + msg.picked : 'This patient');
        $('#' + dlLabelCohortId).text(
          'Cohort (' + n + (n === 1 ? ' patient)' : ' patients)'));
        $root.find('.pp-dl-scope-patient')
          .toggleClass('is-hidden', !single);
        $root.find('.pp-dl-scope-cohort')
          .toggleClass('is-hidden', !(n > 1));
        $root.toggleClass('is-hidden', !single && !(n > 1));
      }
      Shiny.addCustomMessageHandler(dlMenuMsgId, function(msg) {
        if (msg) applyDlMenu(msg, 30);
      });

      // Toggle gear popover open/close
      $(document).on('click', '#' + gearBtnId, function(e) {
        e.stopPropagation();
        var popover = document.getElementById(gearPopoverId);
        if (popover) popover.classList.toggle('is-open');
        $(this).toggleClass('is-active');
      });

      // Close popover when clicking outside it
      $(document).on('click', function(e) {
        var $btn = $('#' + gearBtnId);
        var $pop = $('#' + gearPopoverId);
        if (!$btn.length || !$pop.length) return;
        if (!$btn.is(e.target) && !$btn.has(e.target).length &&
            !$pop.is(e.target) && !$pop.has(e.target).length) {
          $pop.removeClass('is-open');
          $btn.removeClass('is-active');
        }
      });

      // Timeline mode click-through: flip current value on click.
      // Read/write via attr(), not data() — jQuery's .data() caches
      // the initial attribute value and ignores later attr() writes,
      // so subsequent clicks would always read the original mode.
      $(document).on('click',
        '#' + layoutId + ' .pp-popover-toggle[data-tl-mode]',
        function(e) {
          e.stopPropagation();
          if ($(this).attr('data-disabled') === '1') return;
          var cur = $(this).attr('data-tl-mode');
          var next = (cur === 'rday') ? 'date' : 'rday';
          // Optimistic UI update; server re-render will confirm.
          $(this).text(next === 'rday' ? 'Relative day' : 'Date');
          $(this).attr('data-tl-mode', next);
          Shiny.setInputValue(tlModeInputId, next, {priority: 'event'});
        });

      // Pre-treatment history toggle, same flip-on-click pattern.
      $(document).on('click',
        '#' + layoutId + ' .pp-popover-toggle[data-prestudy]',
        function(e) {
          e.stopPropagation();
          var on = $(this).attr('data-prestudy') === '1';
          var next = !on;
          $(this).text(next ? 'Full history' : 'Screening only');
          $(this).attr('data-prestudy', next ? '1' : '0');
          Shiny.setInputValue(prestudyInputId, next,
                              {priority: 'event'});
        });

      // Value-line smoothing toggle, same flip-on-click pattern.
      $(document).on('click',
        '#' + layoutId + ' .pp-popover-toggle[data-smooth]',
        function(e) {
          e.stopPropagation();
          var cur = $(this).attr('data-smooth');
          var next = (cur === 'off') ? 'auto' : 'off';
          $(this).text(next === 'off' ? 'Straight' : 'Smooth');
          $(this).attr('data-smooth', next);
          Shiny.setInputValue(smoothInputId, next, {priority: 'event'});
        });

      // Picking a patient, and when the server hears about it.
      //
      // Walking the list with the arrow keys moves a class; loading
      // the patient is a full profile render. Held down, the key
      // repeats every ~30ms and the render takes longer than that, so
      // every row on the way was requested, and each answer arrived
      // late and dragged the selection back to a row already left --
      // walking ten rows landed you somewhere in the middle of them.
      //
      // So the list is walked locally and only where you STOP is sent.
      // A click sends at once: it is one deliberate landing, and
      // waiting a quarter second to react to a click reads as lag.
      var PICK_SETTLE_MS = 250;
      var pickTimer = null;
      var pickPending = null;

      function sendPick(id, now) {
        if (pickTimer) { clearTimeout(pickTimer); pickTimer = null; }
        if (now) {
          pickPending = null;
          Shiny.setInputValue(pickSubjectInputId, id,
                              {priority: 'event'});
          return;
        }
        // What the list is showing but the server has not been told
        // about. It is also the guard on the echo below: a confirming
        // message for the row you were on two presses ago must not
        // move the class off the row you are on now.
        pickPending = id;
        pickTimer = setTimeout(function() {
          pickTimer = null;
          pickPending = null;
          Shiny.setInputValue(pickSubjectInputId, id,
                              {priority: 'event'});
        }, PICK_SETTLE_MS);
      }

      // Patient row click: pick that patient. The selected class is
      // applied optimistically so the list responds at click speed --
      // the server re-renders it from r_subject() a flush later and
      // agrees, since the click is the only thing that sets it.
      $(document).on('click', '#' + layoutId + ' .pp-pt', function() {
        var id = $(this).attr('data-usubjid');
        if (!id) return;
        // Focus the list, so the next arrow key walks from here rather
        // than doing nothing.
        var well = document.getElementById(cohortWellId);
        if (well) well.focus({preventScroll: true});
        $('#' + layoutId + ' .pp-pt').removeClass('is-selected');
        $(this).addClass('is-selected');
        sendPick(id, true);
      });

      // Hold the last frame while a panel re-renders.
      //
      // The slot is a renderUI, so a settings change empties the panel
      // and refills it, and for the ~40ms until echarts paints the new
      // canvas the box is blank white. The redraw is cheap (4ms on the
      // server) -- it is the blink that reads badly, not the wait.
      //
      // So the outgoing panel is photographed and the photograph is
      // left in place until the replacement has painted, then faded
      // out. Nothing about the data changes: this is one <canvas> of
      // pixels sitting on top of the panel, which is why it needs no
      // second data path and cannot disagree with the render.
      function ghostPanel(panel) {
        if (panel.__ghost) return;
        // The CHART only, never the header. The header is plain DOM --
        // it rebuilds within a frame and never flashes -- and it holds
        // the find box: a photograph over it would show the OLD search
        // text for 250ms, so the characters you just typed would
        // appear to vanish. That is worse than the blink this is here
        // to hide. Leaving the header live also keeps the box
        // clickable throughout.
        var body = panel.querySelector('.pp-chart-body');
        if (!body) return;
        // Not panel.getBoundingClientRect(): Shiny styles its output
        // wrappers `div:where(.shiny-html-output):has(> *) { display:
        // contents }` (the rule behind blockr.ui#41), so the slot div
        // generates NO BOX -- zero width, zero height, no offsetParent,
        // and `position: relative` on it or its parent does nothing.
        // The body is a real box; measure that.
        var box = body.getBoundingClientRect();
        // Nothing painted yet -- a slot's first render has no frame to
        // hold, and photographing it produced a 0x0 overlay.
        if (!box.width || !box.height) return;

        var ghost = document.createElement('div');
        ghost.className = 'pp-chart-ghost';
        // Fixed to the viewport and parented to <body>, because every
        // ancestor between here and there is display:contents and
        // cannot contain an absolutely positioned child.
        ghost.style.top = box.top + 'px';
        ghost.style.left = box.left + 'px';
        ghost.style.width = box.width + 'px';
        ghost.style.height = box.height + 'px';
        ghost.innerHTML = body.innerHTML;
        // innerHTML clones a canvas ELEMENT but none of its pixels, so
        // an echarts chart would come back blank -- which is the very
        // white box this exists to hide. Copy the bitmaps across.
        var src = body.querySelectorAll('canvas');
        var dst = ghost.querySelectorAll('canvas');
        for (var i = 0; i < src.length && i < dst.length; i++) {
          try {
            dst[i].width = src[i].width;
            dst[i].height = src[i].height;
            dst[i].getContext('2d').drawImage(src[i], 0, 0);
          } catch (e) { /* tainted or zero-sized; the fade still helps */ }
        }
        document.body.appendChild(ghost);
        panel.__ghost = ghost;
      }

      function dropGhost(ghost) {
        if (ghost && ghost.parentNode) ghost.parentNode.removeChild(ghost);
      }

      function unghostPanel(panel) {
        var ghost = panel.__ghost;
        if (!ghost) return;
        panel.__ghost = null;

        function fade() {
          requestAnimationFrame(function() {
            ghost.classList.add('is-gone');
            setTimeout(function() { dropGhost(ghost); }, 200);
          });
        }

        // Wait for the replacement to have PAINTED, not merely to
        // exist.
        //
        // This used to fade two frames after the HTML arrived, on the
        // theory that one frame lays the DOM out and the next paints
        // it. Measured, echarts creates its canvas ~150ms after the
        // value event, so the photograph was fading over an empty box
        // and the new chart appeared into the gap: old chart, nothing,
        // new chart. Holding until a canvas exists makes it one
        // cross-fade.
        //
        // A panel that draws no widget (an empty-state message) never
        // grows a canvas, so it fades at once, and the deadline stops
        // a chart that fails to paint from leaving a stale photograph
        // of the last patient over it.
        if (!panel.querySelector('.html-widget')) { fade(); return; }
        var until = Date.now() + 1500;
        (function waitForPaint() {
          if (panel.querySelector('canvas') || Date.now() > until) {
            fade();
            return;
          }
          requestAnimationFrame(waitForPaint);
        })();
      }

      $(document).on('shiny:recalculating', function(e) {
        var el = e.target;
        if (el && el.id && el.id.indexOf('viz_slot_') >= 0) {
          ghostPanel(el);
        }
      });
      $(document).on('shiny:value shiny:error', function(e) {
        var el = e.target;
        if (el && el.id && el.id.indexOf('viz_slot_') >= 0) {
          unghostPanel(el);
        }
      });
      // A fixed overlay does not scroll with the page, so a scroll
      // while one is up would leave it hanging over the wrong content.
      // The panel underneath is live; dropping the photograph early
      // costs at worst the blink it was hiding.
      window.addEventListener('scroll', function() {
        var g = document.querySelectorAll('.pp-chart-ghost');
        for (var i = 0; i < g.length; i++) dropGhost(g[i]);
      }, true);

      // Dragging a panel by its own header.
      //
      // Pointer events, not HTML5 drag-and-drop: native DnD has no
      // autoscroll of its own (a stack of 400px charts needs one), no
      // control over the drag image, and cannot be driven by synthetic
      // input, so it cannot be tested.
      //
      // Positions come from panelRect(), not from the slot elements
      // themselves: Shiny styles its output wrappers display:contents
      // (blockr.ui#41), so every panel div measures 0x0 and only its
      // children have boxes. The ghost overlay learned this first.
      var dragLine = null;

      // Where a panel actually IS on screen.
      //
      // Not slot.getBoundingClientRect(): Shiny styles its output
      // wrappers `div:where(.shiny-html-output):has(> *) { display:
      // contents }` (blockr.ui#41), so a slot generates no box at all
      // -- zero width, zero height. Its children are the real boxes, so
      // the panel's rect is their union.
      function panelRect(slot){
        var box = null;
        for (var i = 0; i < slot.children.length; i++) {
          var r = slot.children[i].getBoundingClientRect();
          if (!r.width || !r.height) continue;
          box = box ? {
            top: Math.min(box.top, r.top),
            left: Math.min(box.left, r.left),
            right: Math.max(box.right, r.right),
            bottom: Math.max(box.bottom, r.bottom)
          } : {top: r.top, left: r.left, right: r.right, bottom: r.bottom};
        }
        return box;
      }

      function slotPanels(){
        var area = document.getElementById(chartAreaId);
        if (!area) return [];
        return [].slice.call(area.querySelectorAll('[id*=viz_slot_]'))
          .map(function(el){
            var box = panelRect(el);
            return box ? {el: el, box: box,
                          id: el.id.replace(/^.*viz_slot_/, '')} : null;
          }).filter(Boolean);
      }

      function showLine(y, left, right){
        if (!dragLine) {
          dragLine = document.createElement('div');
          dragLine.className = 'pp-drop-line';
          document.body.appendChild(dragLine);
        }
        dragLine.style.top = (y - 1) + 'px';
        dragLine.style.left = left + 'px';
        dragLine.style.width = (right - left) + 'px';
      }
      function hideLine(){
        if (dragLine && dragLine.parentNode) {
          dragLine.parentNode.removeChild(dragLine);
        }
        dragLine = null;
      }

      $(document).on('mousedown', '#' + layoutId + ' .pp-chart-header',
        function(e) {
          // Everything clickable in the header keeps its click.
          if (e.target.closest('button, a, input, select, details, ' +
                               '.pp-ctrl-chip, .pp-ctrl-pill, ' +
                               '.pp-ctrl-radio, .pp-ctrl-toggle')) return;
          var panels = slotPanels();
          if (panels.length < 2) return;
          var slot = e.target.closest('[id*=viz_slot_]');
          if (!slot) return;
          var from = panels.findIndex(function(p){ return p.el === slot; });
          if (from < 0) return;
          e.preventDefault();

          var area = document.getElementById(chartAreaId);
          slot.classList.add('pp-is-dragging');
          document.body.classList.add('pp-dragging');
          var to = from;

          function place(clientY){
            var live = slotPanels();
            to = live.length;
            for (var i = 0; i < live.length; i++) {
              var b = live[i].box;
              if (clientY < b.top + (b.bottom - b.top) / 2) { to = i; break; }
            }
            var edge = to < live.length
              ? {y: live[to].box.top - 4, b: live[to].box}
              : {y: live[live.length-1].box.bottom + 4,
                 b: live[live.length-1].box};
            showLine(edge.y, edge.b.left, edge.b.right);
          }
          place(e.clientY);

          function move(ev){
            place(ev.clientY);
            // Autoscroll: a panel can be taller than the viewport, so
            // without this the bottom half of a long stack is
            // unreachable while the button is held.
            if (!area) return;
            var r = area.getBoundingClientRect();
            if (ev.clientY < r.top + 40) area.scrollTop -= 12;
            else if (ev.clientY > r.bottom - 40) area.scrollTop += 12;
          }
          function up(){
            document.removeEventListener('mousemove', move);
            document.removeEventListener('mouseup', up);
            hideLine();
            slot.classList.remove('pp-is-dragging');
            document.body.classList.remove('pp-dragging');
            var ids = slotPanels().map(function(p){ return p.id; });
            var t = to;
            if (t !== from && t !== from + 1) {
              var moved = ids.splice(from, 1)[0];
              if (from < t) t -= 1;
              ids.splice(t, 0, moved);
              Shiny.setInputValue(reorderInputId, ids,
                                  {priority: 'event'});
            }
          }
          document.addEventListener('mousemove', move);
          document.addEventListener('mouseup', up);
        });

      // Which panel the cohort strip is drawing.
      //
      // REMEMBERED and re-applied, not painted once. The message fires
      // when the source changes, and the one at boot arrives before
      // the chart area has rendered a single panel -- so the mark went
      // nowhere and no panel ever carried it. Exactly the bug the
      // picker's ticks had.
      var bandVizId = '';

      function paintBandTag(){
        var area = document.getElementById(chartAreaId);
        if (!area) return;
        area.querySelectorAll('[id*=viz_slot_]').forEach(function(el){
          el.classList.toggle('pp-is-band',
            !!bandVizId && el.id.indexOf('viz_slot_' + bandVizId) >= 0);
        });
      }

      Shiny.addCustomMessageHandler(syncBandMsgId, function(msg) {
        bandVizId = (msg && msg.viz_id) || '';
        paintBandTag();
      });

      // The + button and its picker.
      //
      // Filtering is client-side over rows the server rendered once:
      // the catalogue is a few dozen rows, it is all in the DOM
      // already, and a round trip per keystroke would cost the caret
      // for nothing (the AE find box learned that the hard way).
      //
      // Clicking a row sends the SAME inputs the sidebar's card list
      // sent -- toggle_viz for a panel, pick_param for a parameter --
      // so the server's selection logic is untouched. pick_param
      // already does the right thing for a parameter: it adds it to a
      // card that is on the profile, or opens that card showing only
      // that parameter.
      var addPopId = ns('pp_panels');
      // The sidebar's own box: there is no second search any more.
      var addInputId = ns('search');

      function addRows(){
        return document.querySelectorAll('#' + addPopId + ' .pp-add-row');
      }

      // What is on the profile, REMEMBERED.
      //
      // The ticks used to be painted only when sync_selected arrived,
      // which is on change -- and the one at boot lands before the
      // picker's rows exist, so its marks went nowhere. Opening the
      // picker then showed nothing ticked, and clicking a panel that
      // WAS on the profile removed it -- the row offered to add and did
      // the opposite. Same fix the parameter check marks already use:
      // keep the last state and re-apply it whenever there is something
      // to apply it to.
      var lastSelected = [];
      var lastParamOn = [];

      // The picker's own ordered list of what is on the profile.
      //
      // Built here rather than server-side because the order changes on
      // every drag, and the catalogue below it is rendered once per
      // study. Labels come from the catalogue's own rows, so there is
      // one source for them.
      function addLabels(){
        var map = {};
        addRows().forEach(function(r){
          if (r.getAttribute('data-kind') !== 'panel') return;
          var dot = r.querySelector('.pp-add-dot');
          var code = r.querySelector('.pp-add-code');
          map[r.getAttribute('data-viz-id')] = {
            label: r.querySelector('.pp-add-name').textContent,
            colour: dot ? dot.style.background : '',
            code: code ? code.textContent : ''
          };
        });
        return map;
      }

      function renderAddOn(){
        var host = document.getElementById(addOnId);
        if (!host) return;
        var map = addLabels();
        var want = lastSelected.filter(function(id){ return !!map[id]; });
        // A repaint that would draw the same rows is skipped.
        //
        // A pick moves a row optimistically and animates it; the
        // server's sync_selected lands a tenth of a second later
        // saying the same thing, and rebuilding the list there tore
        // the row out mid-travel -- the tint measured 30ms instead of
        // 600. Comparing first costs one array walk over at most a
        // dozen ids.
        var have = [].slice.call(host.children).map(function(r){
          return r.getAttribute('data-viz-id'); });
        var same = have.length === want.length &&
          have.every(function(v, i){ return v === want[i]; });
        // The count on the heading, set where the list is painted.
        // It was set from sync_selected, which fires on CHANGE -- so
        // at boot it landed before the heading existed and the profile
        // said nothing until you touched something.
        $('#' + layoutId + ' .pp-add-n').text(want.length || '');
        if (same) { addOnVisibility(host); return; }
        host.innerHTML = '';
        want.forEach(function(id){
          var m = map[id];
          if (!m) return;
          var row = document.createElement('div');
          row.className = 'pp-add-ord';
          row.setAttribute('data-viz-id', id);
          var grip = document.createElement('span');
          grip.className = 'pp-add-ord-grip';
          grip.innerHTML = GRIP_SVG;
          // A panel is a colour, a parameter is its code -- the
          // same two row styles the catalogue below uses, so a thing
          // looks the same wherever it currently sits.
          var mark;
          if (m.code) {
            mark = document.createElement('span');
            mark.className = 'pp-add-ord-code';
            mark.textContent = m.code;
          } else {
            mark = document.createElement('span');
            mark.className = 'pp-add-dot';
            mark.style.background = m.colour;
          }
          var name = document.createElement('span');
          name.className = 'pp-add-name';
          name.textContent = m.label;
          var x = document.createElement('button');
          x.className = 'pp-add-ord-x';
          x.type = 'button';
          x.title = 'Remove';
          x.innerHTML = '&times;';
          row.appendChild(grip); row.appendChild(mark);
          row.appendChild(name); row.appendChild(x);
          host.appendChild(row);
        });
        addOnVisibility(host);
      }

      // Hidden only when there is nothing on the profile. It used to
      // hide while searching too, when it lived in a menu and the
      // results were the whole answer; in the sidebar it is the
      // profile itself and a search is a thing happening beneath it.
      function addOnVisibility(host){
        var wrap = host.parentElement;
        if (!wrap) return;
        wrap.classList.toggle('is-hidden', !lastSelected.length);
      }

      // Picking moves a thing, it does not tick it.
      //
      // A row you pick belongs in the list above; leaving it in the
      // catalogue with a mark on it put the same parameter on screen
      // twice, and the tick then said what the list above already
      // said. So it travels, and the rows it left close the gap behind
      // it. The motion is the whole explanation.
      //
      // FLIP: measure both lists before the DOM changes, redraw, then
      // play the difference back to zero -- the browser animates a
      // transform and never the layout. The repaint is optimistic, so
      // the before-rects are taken in the same click and the server's
      // sync_selected lands later to find everything already in place.
      //
      // The tick survives for one case: a SEARCH still lists what is
      // already on the profile, and while searching the list above is
      // hidden, so nothing else would say so.
      var ADD_FLIP_MS = 190;
      function addReduceMotion(){
        return !!(window.matchMedia &&
          window.matchMedia('(prefers-reduced-motion: reduce)').matches);
      }
      function addFlipRects(){
        var m = {}, pop = document.getElementById(addPopId);
        if (!pop) return m;
        pop.querySelectorAll('.pp-add-ord, .pp-add-row')
          .forEach(function(r){
            var b = r.getBoundingClientRect();
            if (b.height) m[r.getAttribute('data-viz-id')] = b;
          });
        return m;
      }
      function addFlipPlay(before, landed){
        var pop = document.getElementById(addPopId);
        if (!pop) return;
        var reduce = addReduceMotion();
        if (!reduce) {
          pop.querySelectorAll('.pp-add-ord, .pp-add-row')
            .forEach(function(r){
              var b = before[r.getAttribute('data-viz-id')];
              if (!b) return;
              var a = r.getBoundingClientRect();
              if (!a.height) return;
              var dy = b.top - a.top, dx = b.left - a.left;
              if (!dy && !dx) return;
              r.style.transition = 'none';
              r.style.transform =
                'translate(' + dx + 'px,' + dy + 'px)';
              requestAnimationFrame(function(){
                r.style.transition =
                  'transform ' + ADD_FLIP_MS + 'ms ' +
                  'cubic-bezier(.2,.7,.3,1)';
                r.style.transform = '';
              });
            });
        }
        // And a moment of tint where it landed. On a long profile the
        // list above can be scrolled past its box, and the tint is the
        // part that survives arriving off-screen.
        if (!landed) return;
        var t = pop.querySelector(
          '.pp-add-ord[data-viz-id=' + JSON.stringify(landed) + ']');
        if (!t) return;
        t.classList.add('is-landed');
        window.setTimeout(function(){
          t.classList.remove('is-landed');
        }, reduce ? 500 : ADD_FLIP_MS + 420);
      }

      // Reordering inside the picker. The same pointer drag the panels
      // themselves use, over 28px rows instead of 400px charts -- which
      // is the whole reason to offer it here as well.
      $(document).on('mousedown', '#' + addOnId + ' .pp-add-ord',
        function(e) {
          if (e.target.closest('button')) return;
          e.preventDefault();
          var host = document.getElementById(addOnId);
          var rows = [].slice.call(host.children);
          var from = rows.indexOf(this);
          if (from < 0 || rows.length < 2) return;
          this.classList.add('is-dragging');
          var to = from;

          function mark(y){
            rows.forEach(function(r){
              r.classList.remove('is-over', 'is-over-last');
            });
            to = rows.length;
            for (var i = 0; i < rows.length; i++) {
              if (i === from) continue;
              var bx = rows[i].getBoundingClientRect();
              if (y < bx.top + bx.height / 2) {
                rows[i].classList.add('is-over'); to = i; return;
              }
            }
            rows[rows.length - 1].classList.add('is-over-last');
          }
          mark(e.clientY);

          function move(ev){ mark(ev.clientY); }
          function up(){
            document.removeEventListener('mousemove', move);
            document.removeEventListener('mouseup', up);
            rows.forEach(function(r){
              r.classList.remove('is-dragging', 'is-over',
                                 'is-over-last');
            });
            if (to !== from && to !== from + 1) {
              var ids = rows.map(function(r){
                return r.getAttribute('data-viz-id'); });
              var t = to;
              var moved = ids.splice(from, 1)[0];
              if (from < t) t -= 1;
              ids.splice(t, 0, moved);
              lastSelected = ids;
              renderAddOn();
              Shiny.setInputValue(reorderInputId, ids,
                                  {priority: 'event'});
            }
          }
          document.addEventListener('mousemove', move);
          document.addEventListener('mouseup', up);
        });

      $(document).on('click', '#' + addOnId + ' .pp-add-ord-x',
        function(e) {
          e.stopPropagation();
          var id = this.parentElement.getAttribute('data-viz-id');
          // The same move, downwards: it travels back to where the
          // catalogue keeps it, which is where you would look for it
          // if you wanted it again.
          var before = addFlipRects();
          lastSelected = lastSelected.filter(function(x){
            return x !== id; });
          $('#' + layoutId + ' .pp-add-n').text(lastSelected.length || '');
          filterAdd();
          addFlipPlay(before, null);
          Shiny.setInputValue(toggleInputId, id, {priority: 'event'});
        });

      function paintAddTicks(){
        addRows().forEach(function(r){
          var id = r.getAttribute('data-viz-id');
          var on = r.getAttribute('data-kind') === 'param'
            ? lastParamOn.indexOf(
                id + '@@' + r.getAttribute('data-paramcd')) >= 0
            : lastSelected.indexOf(id) >= 0;
          r.classList.toggle('is-on', on);
        });
        renderAddOn();
      }
      function filterAdd(){
        renderAddOn();
        var pop = document.getElementById(addPopId);
        if (!pop) return;
        var inp = document.getElementById(addInputId);
        var q = (inp ? inp.value : '').trim().toLowerCase();
        var shown = 0;
        addRows().forEach(function(r){
          var hay = r.getAttribute('data-search-text') || '';
          // No query, no catalogue. What the sidebar shows then is the
          // profile you have, which is the list above this one; sixty
          // rows of everything a study measures is not a menu, and it
          // is the reason this list left the sidebar in the first
          // place. Searching shows every match, on the profile or not,
          // so a hit is never missing.
          var ok = q ? hay.indexOf(q) >= 0 : false;
          r.classList.toggle('is-hidden', !ok);
          if (ok) shown++;
        });
        // A group heading with nothing under it is noise. Scoped to
        // the catalogue: the ordered list's own heading is followed by
        // a container rather than by rows, and this walk would always
        // decide it was empty.
        pop.querySelectorAll('.pp-add-results .pp-add-group')
          .forEach(function(g){
          var any = false, n = g.nextElementSibling;
          while (n && n.classList.contains('pp-add-row')) {
            if (!n.classList.contains('is-hidden')) { any = true; break; }
            n = n.nextElementSibling;
          }
          g.classList.toggle('is-hidden', !any || !!q);
        });
        var none = pop.querySelector('.pp-add-none');
        // Only when the query found nothing ANYWHERE. It counted panel
        // hits alone, so searching for a patient id -- which no panel
        // matches -- printed an empty-result line directly above the
        // patient it had just found.
        if (none) {
          var pats = document.querySelectorAll(
            '#' + layoutId + ' .pp-pt:not(.is-filtered-out)').length;
          none.classList.toggle('is-shown', !!q && shown === 0 && !pats);
        }
      }

      $(document).on('click', '#' + layoutId + ' .pp-add-row', function(e){
        e.stopPropagation();
        var kind = this.getAttribute('data-kind');
        var vizId = this.getAttribute('data-viz-id');
        // Optimistic, so the tick lands at click speed; the server
        // confirms through sync_selected a flush later. The remembered
        // state moves with it, or reopening the picker would repaint
        // the row back to what it was.
        var nowOn = !this.classList.contains('is-on');
        var before = addFlipRects();
        this.classList.toggle('is-on', nowOn);
        if (kind === 'panel') {
          // The query has done its job, and it is also hiding the
          // cohort: one box filters both lists, so a query left
          // standing after the pick leaves the patients filtered away
          // by a search you have already acted on.
          var box = document.getElementById(addInputId);
          if (box && box.value) {
            box.value = '';
            window.setTimeout(function(){
              $(box).trigger('input');
            }, 0);
          }
          var at = lastSelected.indexOf(vizId);
          if (nowOn && at < 0) lastSelected = lastSelected.concat([vizId]);
          if (!nowOn && at >= 0) lastSelected = lastSelected.filter(
            function(x){ return x !== vizId; });
          $('#' + layoutId + ' .pp-add-n').text(lastSelected.length || '');
          // Both lists, now: the catalogue drops what it just handed
          // upwards. filterAdd() repaints the list above as well.
          filterAdd();
          addFlipPlay(before, nowOn ? vizId : null);
        }
        if (kind === 'param') {
          Shiny.setInputValue(pickParamInputId, {
            viz_id: vizId, paramcd: this.getAttribute('data-paramcd')
          }, {priority: 'event'});
        } else {
          Shiny.setInputValue(toggleInputId, vizId, {priority: 'event'});
        }
      });

      // How tall the cohort list is allowed to be.
      //
      // The CSS falls back to the viewport, which is right only when
      // the block runs to the bottom of the window. It does not: the
      // block is a dock panel, and dockview gives that panel a
      // definite height (an inline px height on the render overlay,
      // and a pane inside it with overflow:auto). Split the view, or
      // put the profile in a shorter panel, and a viewport-sized well
      // overflowed its panel -- measured, 600px of list in a 420px
      // panel, which the pane then scrolled. Two scrollbars, the outer
      // one hiding the inner one.
      //
      // So the box the block was GIVEN decides: the first scrolling
      // ancestor, or the window when there is none (the block on a
      // plain page). Everything above the well inside that box --
      // header, search, caption -- is measured rather than assumed,
      // so a caption that wraps to two lines takes its room out of
      // the list instead of pushing it through the bottom.
      var WELL_GAP = 16;     // breathing room under the last row
      var WELL_FLOOR = 132;  // 3 rows; below this nothing useful fits

      function wellHost(el) {
        var n = el.parentElement;
        while (n && n !== document.body) {
          var cs = window.getComputedStyle(n);
          if (cs.overflowY === 'auto' || cs.overflowY === 'scroll') {
            return n;
          }
          n = n.parentElement;
        }
        return null;
      }

      function sizeWell() {
        var well = document.getElementById(cohortWellId);
        if (!well || !well.offsetParent) return;
        var host = wellHost(well);
        var wellTop = well.getBoundingClientRect().top;
        var avail;
        if (host) {
          var hostTop = host.getBoundingClientRect().top;
          avail = host.clientHeight - (wellTop - hostTop + host.scrollTop);
        } else {
          avail = window.innerHeight - wellTop;
        }
        avail = Math.max(WELL_FLOOR, Math.round(avail - WELL_GAP));
        // Only when it actually moves: this runs from a ResizeObserver
        // that the assignment itself can wake.
        if (Math.abs((parseFloat(well.style.maxHeight) || 0) - avail) < 2) {
          return;
        }
        well.style.maxHeight = avail + 'px';
      }

      if (window.ResizeObserver) {
        var wellRO = new ResizeObserver(function() {
          window.requestAnimationFrame(sizeWell);
        });
        var wellWatch = function() {
          var well = document.getElementById(cohortWellId);
          if (!well) return;
          wellRO.disconnect();
          var host = wellHost(well);
          if (host) wellRO.observe(host);
          if (well.parentElement) wellRO.observe(well.parentElement);
          sizeWell();
        };
        // Attached when the list appears, not when the script runs:
        // the block renders after this, so the first attempt found no
        // well, returned, and left nothing observing the panel. The
        // window resize still worked, which is what made it look like
        // it was wired up.
        $(document).on('shiny:value', function(e) {
          if (!e.name) return;
          // The caption row is a renderUI above the well, so what is
          // left for the list changes with the band it names.
          if (e.name.indexOf('cohort_') >= 0 ||
              e.name.indexOf('result') >= 0) {
            window.setTimeout(wellWatch, 0);
          }
        });
        $(window).on('resize', sizeWell);
        setTimeout(wellWatch, 0);
      }

      // Walking the cohort from the keyboard.
      //
      // This replaced the header's next/previous buttons. Scoped to
      // the cohort list rather than bound to the document, for two
      // reasons that are not style: the AE find box lives in the same
      // block and its arrow keys have to move the caret, and a
      // document-level handler would eat the page's own scrolling.
      //
      // The list takes focus on a click, so the ordinary flow -- click
      // a patient, then walk -- needs no second gesture. Home and End
      // jump to the ends, because a 254-row list makes them worth it.
      //
      // THE ROWS ARE THE ORDER. The arrows used to send a direction to
      // the server, which stepped through the cohort in USUBJID order.
      // That is the order the list is in exactly once -- sorted by
      // peak, or with a patient search running, Down went to whoever
      // was next alphabetically, which is a patient somewhere else in
      // the list and sometimes not in it at all. It read as random,
      // and it was: two orders, one of them invisible. The DOM carries
      // both the sort and the filter, so it is walked directly.
      function cohortStep(well, dir, absolute) {
        var rows = [].slice.call(
          well.querySelectorAll('.pp-pt:not(.is-filtered-out)'));
        if (!rows.length) return;
        var at = -1;
        for (var i = 0; i < rows.length; i++) {
          if (rows[i].classList.contains('is-selected')) { at = i; break; }
        }
        var to;
        if (absolute) {
          to = dir > 0 ? rows.length - 1 : 0;
        } else if (at < 0) {
          // Nothing picked yet: step in from the end you came from.
          to = dir > 0 ? 0 : rows.length - 1;
        } else {
          to = (at + dir + rows.length) % rows.length;
        }
        var row = rows[to];
        var id = row.getAttribute('data-usubjid');
        if (!id) return;
        // Moved here rather than waiting for the server to confirm:
        // held down, the arrow repeats faster than the round trip, and
        // every press in the burst would otherwise measure from the
        // same stale anchor and land on the same row.
        rows.forEach(function(r){ r.classList.remove('is-selected'); });
        row.classList.add('is-selected');
        if (row.scrollIntoView) row.scrollIntoView({block: 'nearest'});
        // Home and End are one jump, so they load immediately; the
        // arrows wait for you to stop.
        sendPick(id, !!absolute);
      }

      $(document).on('keydown', '#' + cohortWellId, function(e) {
        var dir = 0, absolute = false;
        if (e.key === 'ArrowDown') dir = 1;
        else if (e.key === 'ArrowUp') dir = -1;
        else if (e.key === 'End') { dir = 1; absolute = true; }
        else if (e.key === 'Home') { dir = -1; absolute = true; }
        else return;
        e.preventDefault();
        cohortStep(this, dir, absolute);
      });

      // The AE band, drawn when its row scrolls into view.
      //
      // The row arrives carrying its geometry (data-band, one
      // x,width,fill triplet per span) and an empty track. Painting
      // all of it server-side put 32,232 rects in the document on a
      // 1251-patient study, and a document that size makes every
      // full-page style recalculation cost 22ms instead of 3ms -- a
      // bill every chart redraw on the board pays, because of the
      // :has(> *) passthrough restyle (blockr.ui#41).
      //
      // Rows are NOT windowed. All 1251 stay in the DOM: the search
      // filter and the scrollbar both have to see the whole cohort.
      var SVGNS = 'http://www.w3.org/2000/svg';
      var bandObserver = null;

      // Spans only: a series row's height comes from its own svg,
      // and a series draws no full-height rect of any kind.
      var BAND_H = cfg.bandH;

      function el(tag, attrs) {
        var e = document.createElementNS(SVGNS, tag);
        for (var k in attrs) e.setAttribute(k, attrs[k]);
        return e;
      }

      // Spans: one rect per event, in the order the source table
      // carries them, later over earlier.
      function drawSpans(frag, spec, h) {
        spec.split(' ').forEach(function(s) {
          var f = s.split(',');
          if (f.length < 3) return;
          frag.appendChild(el('rect', {
            x: f[0], y: '0', width: f[1], height: h,
            fill: f[2], opacity: '0.9'
          }));
        });
      }

      // A value series: the reference limit as a hairline, the values
      // as one polyline, and a tick where the shared scale had to clip
      // one. Same geometry pp_cohort_series_geom() computed -- the
      // client places nothing of its own.
      function drawSeries(frag, spec, row, h) {
        // The reference RANGE, not the ceiling alone.
        //
        // One hairline used to be drawn at the upper limit, which for
        // albumin sits a fifth of the way down the strip with every
        // patient underneath it -- ten values in 2058 reach it. Both
        // edges make the useful claim, which is whether this patient
        // is inside the range, and it is shaded the same green the
        // panel chart shades it with.
        var hi = row.getAttribute('data-limit');
        var lo = row.getAttribute('data-limit-lo');
        if (hi && lo) {
          frag.appendChild(el('rect', {
            x: 0, y: hi, width: 176,
            height: Math.max(0, parseFloat(lo) - parseFloat(hi)),
            fill: 'var(--pp-cohort-ref, rgba(5, 150, 105, 0.10))'
          }));
        } else if (hi || lo) {
          var one = hi || lo;
          frag.appendChild(el('line', {
            x1: 0, y1: one, x2: 176, y2: one,
            stroke: 'var(--pp-cohort-limit, #9ca3af)',
            'stroke-width': '0.75', 'stroke-dasharray': '2 2',
            opacity: '0.75'
          }));
        }
        // One visit is a point, not a line. Drawing nothing would
        // read as no data, which is a different fact.
        var one = row.getAttribute('data-dot');
        if (one) {
          var xy = one.split(',');
          frag.appendChild(el('circle', {
            cx: xy[0], cy: xy[1], r: '1.6',
            fill: 'var(--pp-cohort-line, #2563eb)'
          }));
        } else if (spec) {
          // The `d` arrives interpolated (pp_monotone_path()), so the
          // rounding is computed once on the server and this only
          // draws it. Nothing here decides the shape of a curve.
          frag.appendChild(el('path', {
            d: spec, fill: 'none',
            stroke: 'var(--pp-cohort-line, #2563eb)',
            'stroke-width': '1.1', 'stroke-linejoin': 'round',
            'stroke-linecap': 'round'
          }));
        }
        // Clipped values, ticked at the edge each one ran off.
        var tick = function(attr, y1, y2) {
          var v = row.getAttribute(attr);
          if (!v) return;
          v.split(' ').forEach(function(cx) {
            frag.appendChild(el('line', {
              x1: cx, y1: y1, x2: cx, y2: y2,
              stroke: 'var(--pp-cohort-clip, #dc2626)',
              'stroke-width': '1.2'
            }));
          });
        };
        tick('data-clip', 0, 2.5);
        tick('data-clip-lo', h - 2.5, h);
      }

      function drawBand(row) {
        if (row.getAttribute('data-band-drawn')) return;
        row.setAttribute('data-band-drawn', '1');
        var svg = row.querySelector('.pp-pt-band');
        if (!svg) return;
        var frag = document.createDocumentFragment();
        var spec = row.getAttribute('data-band') || '';
        // The row's own height, not a constant: the two kinds of strip
        // are different heights now (8px of colour, 30px of line), and
        // reading it off the svg is what keeps the ticks and the
        // diamond on the edge they belong to.
        var h = parseFloat(svg.getAttribute('height')) || BAND_H;
        if (row.getAttribute('data-band-kind') === 'series') {
          drawSeries(frag, spec, row, h);
        } else if (spec) {
          drawSpans(frag, spec, h);
        }
        // End of treatment, the same diamond and the same radius the
        // server drew.
        var eot = row.getAttribute('data-eot');
        if (eot) {
          var cx = parseFloat(eot), cy = h / 2,
              rr = Math.min(3, h / 2 + 1);
          frag.appendChild(el('path', {
            d: 'M' + cx + ' ' + (cy - rr) + 'L' + (cx + rr) + ' ' + cy +
               'L' + cx + ' ' + (cy + rr) + 'L' + (cx - rr) + ' ' + cy +
               'Z',
            fill: 'var(--pp-cohort-eot, #6b7280)'
          }));
        }
        svg.appendChild(frag);
      }

      function observeBands() {
        var well = document.getElementById(cohortWellId);
        if (!well) return;
        var rows = well.querySelectorAll('.pp-pt');
        // No IntersectionObserver: draw the lot. A slower first paint
        // beats a list of empty tracks.
        if (!window.IntersectionObserver) {
          Array.prototype.forEach.call(rows, drawBand);
          return;
        }
        if (bandObserver) bandObserver.disconnect();
        bandObserver = new IntersectionObserver(function(entries) {
          entries.forEach(function(e) {
            if (!e.isIntersecting) return;
            drawBand(e.target);
            bandObserver.unobserve(e.target);
          });
        // A row is 44px, so 200px draws about four rows past either
        // edge -- enough that a scroll never uncovers a bare track.
        }, {root: well, rootMargin: '200px 0px'});
        Array.prototype.forEach.call(rows, function(r) {
          bandObserver.observe(r);
        });
      }

      // Every cohort re-render brings undrawn rows. applyFilter() runs
      // too, so a live query survives a re-render -- the treatment the
      // panel list already had, which the cohort list never got.
      $(document).on('shiny:value', function(e) {
        if (e.name && e.name.indexOf('sidebar_cohort') >= 0) {
          setTimeout(function() { observeBands(); applyFilter(); }, 0);
        }
        // The panel slot that owns the find box has just been
        // replaced; put the caret back in it.
        if (e.name && e.name.indexOf('viz_slot_') >= 0) {
          setTimeout(function(){ restoreSearch(); paintBandTag(); }, 0);
        }
        // The whole stack was rebuilt: a patient switch, or the first
        // render of all.
        if (e.name && e.name.indexOf('chart_area') >= 0) {
          setTimeout(paintBandTag, 0);
        }
        // A fresh catalogue arrives with nothing ticked.
        if (e.name && e.name.indexOf('panel_picker') >= 0) {
          setTimeout(function(){ paintAddTicks(); filterAdd(); }, 0);
        }
      });

      // Sort key: the house click-through pill. Its own handler
      // rather than the viz one, because that sends {viz_id, param,
      // value} to a viz-settings observer and this is neither.
      $(document).on('click', '#' + cohortSortById, function(e) {
        e.stopPropagation();
        var $by = $(this);
        var values = $by.data('values');
        var labels = $by.data('labels');
        if (!values || !values.length) return;
        var idx = (parseInt($by.attr('data-index'), 10) + 1) %
          values.length;
        $by.attr('data-index', idx);
        // Only the key, not the whole control: the word before it
        // and the caret after it are markup, and .text() eats them.
        $by.find('b').text(labels[idx]);
        $by.attr('title',
          'Sort by ' + labels[(idx + 1) % values.length]);
        Shiny.setInputValue(cohortSortInputId, values[idx],
                            {priority: 'event'});
      });

      // The pick, from anywhere that is not this list. Moves a class;
      // never re-renders the rows.
      Shiny.addCustomMessageHandler(syncSubjectMsgId, function(msg) {
        var id = msg && msg.id ? msg.id : null;
        // While the walk is settling the list is ahead of the server,
        // and this message is the server agreeing with a row that has
        // already been left. Ignore it; the send at the end of the
        // walk brings one that agrees.
        if (pickPending && id !== pickPending) return;
        var $rows = $('#' + layoutId + ' .pp-pt');
        $rows.removeClass('is-selected');
        if (!id) return;
        var $row = $rows.filter('[data-usubjid=' +
          JSON.stringify(id) + ']');
        $row.addClass('is-selected');
        // Bring it back into view: on a long cohort the pick can be
        // hundreds of rows away (the arrow keys walk it there), and a
        // selected row nobody can see is the same as no selection.
        if ($row.length && $row[0].scrollIntoView) {
          $row[0].scrollIntoView({block: 'nearest'});
        }
      });

      // Panel x: remove the viz. Same input as the card, so the server
      // deselects it and the sidebar card slides back to AVAILABLE.
      $(document).on('click', '#' + layoutId + ' .pp-chart-remove',
        function(e) {
          e.stopPropagation();
          var vizId = $(this).attr('data-viz-id');
          if (!vizId) return;
          Shiny.setInputValue(toggleInputId, vizId, {priority: 'event'});
        });

      // Search: client-side filtering across both sections.
      //
      // Match on the card's own data-search-text and mark it with a
      // class -- never on `:visible`. `:visible` is false for every
      // card inside an already-hidden section, so a section hidden by
      // one keystroke could never come back on the next: the SELECTED
      // section stayed collapsed after the query was cleared.
      //
      // The empty-selection hints hide while a query is live: they
      // speak about the full list, not about the matches.
      function applyFilter() {
        var $sidebar = $('#' + sidebarId);
        var query = ($('#' + searchId).val() || '').toLowerCase().trim();

        $('#' + clearBtnId).toggleClass('is-hidden', !query);
        $sidebar.toggleClass('is-searching', !!query);

        // One search box, two tenants. A query hides non-matching
        // PATIENTS in place rather than switching the cohort to a
        // results tree: the list is already flat, so there is nothing
        // to restructure, and the count in the header keeps telling
        // you the size of the cohort rather than of the query.
        $sidebar.find('.pp-pt').each(function() {
          var hay = $(this).attr('data-search-text') || '';
          $(this).toggleClass('is-filtered-out',
            !!query && hay.indexOf(query) === -1);
        });

        // One box, two tenants: the panels above and the patients
        // below. AFTER the rows above, because the empty-result line
        // has to count both lists -- run first, it counted the
        // patients the PREVIOUS query had left standing.
        filterAdd();
        markFirstHit();
      }

      $(document).on('input', '#' + searchId, applyFilter);

      // Clear (x): reset the query and hand back the full list
      $(document).on('click', '#' + clearBtnId, function(e) {
        e.stopPropagation();
        $('#' + searchId).val('').focus();
        applyFilter();
      });

      // Escape clears too, while the box has focus
      // What Enter would take.
      //
      // The first hit in reading order: a panel if the query found
      // one, since panels are listed above, otherwise the first
      // patient still standing. Marked as well as acted on -- a key
      // that does something invisible is a key nobody presses.
      function firstHit(){
        var row = document.querySelector(
          '#' + addPopId + ' .pp-add-row:not(.is-hidden)');
        if (row) return row;
        return document.querySelector(
          '#' + layoutId + ' .pp-pt:not(.is-filtered-out)');
      }

      function markFirstHit(){
        var q = ($('#' + searchId).val() || '').trim();
        $('#' + layoutId + ' .is-enter').removeClass('is-enter');
        if (!q) return;
        var row = firstHit();
        if (row) row.classList.add('is-enter');
      }

      $(document).on('keydown', '#' + searchId, function(e) {
        if (e.key === 'Escape' || e.keyCode === 27) {
          if (!$(this).val()) return;
          e.stopPropagation();
          $(this).val('');
          applyFilter();
          return;
        }
        if (e.key !== 'Enter' && e.keyCode !== 13) return;
        var row = firstHit();
        if (!row) return;
        e.preventDefault();
        row.click();
      });

      // The cohort tag is the sidebar's toggle.
      //
      // It reads "254 patients" and it opens the list of them, which
      // is where a reader is already looking when they want the
      // cohort. It is also what lets the header drop its own picker:
      // with the sidebar shut this is the way back to one.
      function toggleSidebar(force) {
        var sidebar = document.getElementById(sidebarId);
        var layout = document.getElementById(layoutId);
        if (!sidebar || !layout) return;
        var shut = (force === undefined) ?
          !sidebar.classList.contains('collapsed') : force;
        sidebar.classList.toggle('collapsed', shut);
        layout.classList.toggle('sidebar-collapsed', shut);
        $('#' + cohortCountId).toggleClass('is-shut', shut);
      }

      $(document).on('click', '#' + cohortCountId, function(e) {
        e.stopPropagation();
        toggleSidebar();
      });

      // Chip click (checkbox controls)
      $(document).on('click', '#' + layoutId + ' .pp-ctrl-chip', function(e) {
        e.stopPropagation();
        $(this).toggleClass('is-active');
        var vizId = $(this).data('viz-id');
        var param = $(this).data('param');
        var active = [];
        $(this).closest('.pp-ctrl-chips').find('.pp-ctrl-chip.is-active').each(function() {
          active.push($(this).data('value'));
        });
        Shiny.setInputValue(ctrlInputId, {
          viz_id: vizId, param: param, value: active
        }, {priority: 'event'});
      });

      // Toggle click
      $(document).on('click', '#' + layoutId + ' .pp-ctrl-toggle', function(e) {
        e.stopPropagation();
        $(this).toggleClass('is-on');
        var vizId = $(this).data('viz-id');
        var param = $(this).data('param');
        var isOn = $(this).hasClass('is-on');
        Shiny.setInputValue(ctrlInputId, {
          viz_id: vizId, param: param, value: isOn
        }, {priority: 'event'});
      });

      // Click-through pill: advance to the next value, wrapping. The
      // index lives in the attribute, not in jQuery's data cache: the
      // cache is populated once per element and would hand back the
      // boot value on every later click.
      $(document).on('click', '#' + layoutId + ' .pp-ctrl-pill', function(e) {
        var $pill = $(this);
        // The pill is a shared component, and not every one of them
        // belongs to a viz: the cohort sort borrows the look and has
        // its own handler. Without this guard both fire, the index
        // advances twice (skipping a rung) and this one sends an
        // undefined viz_id to the viz-settings observer.
        if (!$pill.data('viz-id')) return;
        e.stopPropagation();
        var values = $pill.data('values');
        var labels = $pill.data('labels');
        if (!values || !values.length) return;
        var idx = (parseInt($pill.attr('data-index'), 10) + 1) %
          values.length;
        $pill.attr('data-index', idx);
        $pill.text(labels[idx]);
        $pill.attr('title',
          'Switch to ' + labels[(idx + 1) % values.length]);
        Shiny.setInputValue(ctrlInputId, {
          viz_id: $pill.data('viz-id'),
          param: $pill.data('param'),
          value: values[idx]
        }, {priority: 'event'});
      });

      // Find box. Debounced, because every keystroke re-renders the
      // panel AND re-derives 254 cohort bands: sending on each one
      // made typing 'pneumonia' nine round trips, of which eight were
      // thrown away. 400ms is a pause between words rather than
      // between characters -- at 250 the panel redrew mid-word and the
      // whole thing read as nervous.
      var searchTimer = null;
      // What the user has typed but the server has not confirmed, and
      // where their caret was. The panel slot is a renderUI, so the
      // confirming render DESTROYS this input and builds a new one --
      // which drops focus mid-word and leaves nothing to backspace
      // into. Remembered here, restored below.
      var searchState = null;

      $(document).on('input',
        '#' + layoutId + ' .pp-ctrl-search-input', function() {
          var vizId = $(this).data('viz-id');
          var param = $(this).data('param');
          var value = $(this).val();
          searchState = {
            vizId: vizId, param: param, value: value,
            caret: this.selectionStart, at: Date.now()
          };
          // The box owns its own text while the user is in it: the
          // server's confirming re-render must not move the caret, so
          // the wrapper is styled optimistically here and the value is
          // never read back off the DOM (see the input-binding echo
          // trap this package has hit before).
          $(this).closest('.pp-ctrl-search')
            .toggleClass('is-active', value.length > 0);
          if (searchTimer) clearTimeout(searchTimer);
          searchTimer = setTimeout(function() {
            searchTimer = null;
            Shiny.setInputValue(ctrlInputId, {
              viz_id: vizId, param: param, value: value
            }, {priority: 'event'});
          }, 400);
        });

      // Track the caret on every move, not just on input: a click or
      // an arrow key between keystrokes moves it, and restoring a
      // stale position would be its own kind of losing your place.
      $(document).on('keyup click',
        '#' + layoutId + ' .pp-ctrl-search-input', function() {
          if (searchState && searchState.vizId === $(this).data('viz-id')) {
            searchState.caret = this.selectionStart;
          }
        });
      // No blur handler, deliberately. The blur that fires when the
      // re-render DESTROYS the input is indistinguishable from the one
      // that fires when the user clicks away, so reading blur as
      // they-left-on-purpose threw away exactly the state the restore
      // needed -- and the typing carried on into the page body.
      // Recency tells the two apart instead; see below.

      // Give the box back after the panel rebuilds under it. The value
      // is the user's in-flight text rather than the server's echo:
      // the two differ by whatever was typed during the round trip,
      // and taking the server's would silently delete those keys.
      // Only just typed counts. A panel that re-renders for its own
      // reasons a minute later -- a patient switch, an upstream filter
      // -- must not yank the caret back into a box the user left long
      // ago, and 2.5s is far longer than the round trip that follows a
      // keystroke and far shorter than any of that.
      var SEARCH_RESTORE_MS = 2500;

      function restoreSearch() {
        if (!searchState) return;
        if (Date.now() - searchState.at > SEARCH_RESTORE_MS) return;
        // JSON.stringify for the quoting, like the drag handler does:
        // the whole script is an R string, so a literal double quote
        // here would end it.
        var sel = '#' + layoutId + ' .pp-ctrl-search-input' +
          '[data-viz-id=' + JSON.stringify(searchState.vizId) + ']';
        var el = document.querySelector(sel);
        if (!el || el === document.activeElement) return;
        if (el.value !== searchState.value) el.value = searchState.value;
        el.focus();
        var at = Math.min(searchState.caret, el.value.length);
        try { el.setSelectionRange(at, at); } catch (e) { /* no-op */ }
      }

      // Clearing, from the box's own x or from the sidebar's echo of
      // the term. Both send the empty string through the one channel
      // the box uses, so there is a single path back to unfiltered.
      $(document).on('click',
        '#' + layoutId + ' .pp-ctrl-search-clear', function(e) {
          e.stopPropagation();
          if (searchTimer) clearTimeout(searchTimer);
          searchTimer = null;
          searchState = null;
          Shiny.setInputValue(ctrlInputId, {
            viz_id: $(this).data('viz-id'),
            param: $(this).data('param'),
            value: ''
          }, {priority: 'event'});
        });
      $(document).on('click',
        '#' + layoutId + ' .pp-cohort-bandcap-find', function(e) {
          e.stopPropagation();
          if (searchTimer) clearTimeout(searchTimer);
          searchTimer = null;
          searchState = null;
          Shiny.setInputValue(ctrlInputId, {
            viz_id: $(this).attr('data-viz-id'),
            param: 'search',
            value: ''
          }, {priority: 'event'});
        });

      // Radio click
      $(document).on('click', '#' + layoutId + ' .pp-ctrl-radio', function(e) {
        e.stopPropagation();
        $(this).siblings('.pp-ctrl-radio').removeClass('is-active');
        $(this).addClass('is-active');
        var vizId = $(this).data('viz-id');
        var param = $(this).data('param');
        var value = $(this).data('value');
        Shiny.setInputValue(ctrlInputId, {
          viz_id: vizId, param: param, value: value
        }, {priority: 'event'});
      });
      Shiny.addCustomMessageHandler(syncParamsMsgId, function(keys) {
        if (!keys) keys = [];
        if (typeof keys === 'string') keys = [keys];
        lastParamOn = keys;
        paintAddTicks();
      });

      // Sync sidebar state from server
      Shiny.addCustomMessageHandler(syncMsgId, function(selected) {
        if (!selected) selected = [];
        if (typeof selected === 'string') selected = [selected];

        // The + button says how many cards are on the profile, and the
        // picker ticks the ones that are.
        lastSelected = selected;
        $('#' + layoutId + ' .pp-add-n').text(selected.length || '');
        paintAddTicks();

        // Cards moved between the sections keep the live query honest
        applyFilter();
      });

      // --- HTML5 Drag and Drop on active list ---
      var $doc = $(document);

      // --- Keep every chart the width of its container --------------
      // echarts sizes its canvas once, when the widget renders, and
      // htmlwidgets only re-sizes it on a WINDOW resize. That covers
      // exactly one of the ways this panel changes width. Collapsing
      // the sidebar, dragging a dock sash, the dock's maximize button,
      // a view switch -- all of them move the container without any
      // window event, and the canvas stays at its old width with dead
      // space beside it. Reported as 'the plot does not resize when
      // the window is maximized'; the window was never the trigger.
      //
      // So watch the CONTAINER, not the window: ONE observer on the
      // chart area, resizing whatever echarts instances are inside it.
      //
      // A ResizeObserver is not a poll -- it costs nothing while
      // nothing resizes, and does not wake on scroll, hover or a
      // tooltip. What it does do is fire once per animation frame
      // while a width is ANIMATING: collapsing the sidebar is a CSS
      // width transition and wakes it ~30 times. Hence the debounce:
      // those 30 wakes settle into one resize pass, because a resize()
      // is a full re-layout of the chart (~3ms each) and running it
      // per frame across a full profile would be the janky option.
      var chartRO = null;
      var chartROEl = null;
      var chartResizeTimer = null;

      function resizeChartsIn(root) {
        if (typeof echarts === 'undefined') return;
        $(root).find('.echarts4r').each(function() {
          var inst = echarts.getInstanceByDom(this);
          if (inst) inst.resize();
        });
      }

      function watchChartArea() {
        if (!window.ResizeObserver) return;
        var el = document.querySelector(
          '#' + layoutId + ' .pp-chart-area'
        );
        // Same element we are already on: nothing to do. This is the
        // common case, so it stays a single id-anchored lookup.
        if (!el || el === chartROEl) return;
        // A different one means the block's UI was re-mounted. An
        // observer keeps a STRONG reference to what it observes, so
        // holding the detached chart area would pin that whole subtree
        // in memory -- and the new one would never be watched, which
        // is the original bug back again on every view switch.
        if (chartRO) chartRO.disconnect();
        chartROEl = el;
        chartRO = new ResizeObserver(function() {
          clearTimeout(chartResizeTimer);
          chartResizeTimer = setTimeout(function() {
            resizeChartsIn(el);
          }, 80);
        });
        chartRO.observe(el);
      }

      // The layout may not be in the document yet when this script
      // runs (the block's UI arrives as one fragment), and in a dock
      // panel it may not arrive for a while -- so try now and again
      // whenever Shiny renders something in here.
      watchChartArea();
      $doc.on('shiny:value', watchChartArea);
    }
  };
})();
