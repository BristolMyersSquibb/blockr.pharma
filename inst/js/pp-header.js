// @ts-check
/* The block's header row: the count of patients that opens the list, the
 * drill-down reset beside it, the download menu and the gear with its tray.
 *
 * The controls are blockr.ui's (Blockr.gearTray, Blockr.segmented,
 * Blockr.checkbox; the download menu is an action_menu() built in R and run
 * by Blockr.actionMenu). This file only feeds them the profile's state.
 *
 * Depends on: pp-core.js, blockr.ui's blockr-ui.js
 */
PatientProfile.part(function(ctx) {
  var ns = ctx.ns;
  var layoutId = ns('pp_layout');
  var sidebarId = ns('pp_sidebar');
  var tlModeInputId = ns('timeline_mode');
  var prestudyInputId = ns('show_prestudy');
  var smoothInputId = ns('smooth_mode');
  var gearBtnId = ns('pp_gear_btn');
  var gearTrayId = ns('pp_gear_tray');
  var gearDisplayId = ns('pp_gear_display');
  var gearStateMsgId = ns('gear_state');
  var cohortCountId = ns('pp_cohort_count');
  var cohortResetId = ns('pp_cohort_reset');
  var cohortSegId = ns('pp_cohort_seg');
  var drillMsgId = ns('drill');
  var undrillInputId = ns('undrill');
  var subjectPickerMsgId = ns('subject_picker');
  var dlMenuMsgId = ns('dl_menu_state');
  var dlRootId = ns('pp_dl_root');
  var dlLabelCohortId = ns('pp_dl_label_cohort');

  /** @param {string} id */
  var byId = function(id) { return document.getElementById(id); };

  // The count and the reset: one segment, written by two messages.
  //
  // `subject_picker` carries the count, `drill` carries what narrowed it,
  // and they arrive in either order. So neither handler paints: they record,
  // and paintCohort() decides. Painting from one handler alone would let a
  // drill that lands first be undone by the count that follows it.
  var cohortN = 0;
  var drillClause = '';

  function paintCohort() {
    var seg = byId(cohortSegId);
    var count = byId(cohortCountId);
    var reset = /** @type {HTMLButtonElement | null} */ (byId(cohortResetId));
    if (!seg || !count || !reset) return;
    var drilled = drillClause.length > 0;

    var n = count.querySelector('.pp-cohort-count-n');
    if (n) {
      n.textContent = cohortN.toLocaleString() +
        (cohortN === 1 ? ' patient' : ' patients');
    }

    // Hidden at zero ONLY when nothing narrowed the list. No patients and no
    // drill is a profile with nothing in it yet. No patients BECAUSE of a
    // drill is a result, and the one state where the way back matters most,
    // so the segment says 0 and the reset stays.
    seg.classList.toggle('is-hidden', !cohortN && !drilled);
    seg.classList.toggle('is-drilled', drilled);

    // Always there, disabled while nothing is drilled, so the segment never
    // changes width under the pointer. Its tooltip names what is UNDONE:
    // the drill filter sits below the global filter, so what comes back is
    // whatever the dashboard shows, never "every patient".
    reset.disabled = !drilled;
    if (drilled) {
      reset.setAttribute('data-blockr-tooltip', 'Reset drill-down: ' + drillClause);
    } else {
      reset.removeAttribute('data-blockr-tooltip');
    }
  }

  Shiny.addCustomMessageHandler(subjectPickerMsgId, function(msg) {
    if (!msg) return;
    cohortN = msg.count || 0;
    paintCohort();
  });

  // The drill. R reads it off the dm it was handed (pp-drill.R) and sends
  // the clause, or '' when the cohort is the whole study. The reset asks R
  // to clear the drill filter; the profile never edits its own data, it
  // asks the block that did.
  Shiny.addCustomMessageHandler(drillMsgId, function(msg) {
    drillClause = (msg && msg.clause) ? String(msg.clause) : '';
    paintCohort();
  });

  $(document).on('click', '#' + cohortResetId, function(e) {
    e.stopPropagation();
    Shiny.setInputValue(undrillInputId, Date.now(), {priority: 'event'});
  });

  // The count is the list's toggle. The chevron points at the edge the list
  // slides from: left while it is open, right while it is shut.
  /** @param {boolean} [force] */
  function toggleSidebar(force) {
    var sidebar = byId(sidebarId);
    var layout = byId(layoutId);
    var count = byId(cohortCountId);
    if (!sidebar || !layout) return;
    var shut = (force === undefined) ?
      !sidebar.classList.contains('collapsed') : force;
    sidebar.classList.toggle('collapsed', shut);
    layout.classList.toggle('sidebar-collapsed', shut);
    if (count) {
      count.classList.toggle('is-shut', shut);
      count.setAttribute('data-blockr-tooltip',
        shut ? 'Show the list of patients' : 'Hide the list of patients');
    }
  }

  $(document).on('click', '#' + cohortCountId, function(e) {
    e.stopPropagation();
    toggleSidebar();
  });

  // The slot words of the empty states ("Search for one", "Add one"):
  // both are answered from the sidebar's search, so open the list and put
  // the cursor there. Scoped to this block's layout, since the handler is
  // on the document and a board can hold two profiles.
  $(document).on('click', '.pp-open-search', function(e) {
    if (!$(this).closest('#' + layoutId).length) return;
    e.stopPropagation();
    toggleSidebar(false);
    var search = byId(ns('search'));
    if (search) search.focus();
  });

  // The download menu. Rendered once in R; this names the cohort's size and
  // hides the section that does not apply -- the patient's while nobody is
  // picked, the cohort's while it is one patient -- and the whole menu when
  // neither does. Retried briefly, because the first message can arrive
  // before the block's markup does.
  /** @param {{ single: boolean, n: number }} msg @param {number} tries */
  function applyDlMenu(msg, tries) {
    var root = byId(dlRootId);
    if (!root) {
      if (tries > 0) setTimeout(function() { applyDlMenu(msg, tries - 1); }, 100);
      return;
    }
    var single = !!msg.single;
    var n = msg.n || 0;
    var label = byId(dlLabelCohortId);
    if (label) label.textContent = 'Cohort · ' + n.toLocaleString() + (n === 1 ? ' patient' : ' patients');
    /** @type {Record<string, boolean>} */
    var show = { patient: single, cohort: n > 1 };
    root.querySelectorAll('[data-scope]').forEach(function(el) {
      /** @type {HTMLElement} */ (el).hidden = !show[el.getAttribute('data-scope') || ''];
    });
    root.hidden = !single && !(n > 1);
  }
  Shiny.addCustomMessageHandler(dlMenuMsgId, function(msg) {
    if (msg) applyDlMenu(msg, 30);
  });

  // The gear and its tray: blockr.ui's, in flow under the header row.
  var gearBtn = byId(gearBtnId);
  var gearTray = byId(gearTrayId);
  if (gearBtn && gearTray && window.Blockr && Blockr.gearTray) {
    Blockr.gearTray(gearTray, gearBtn, { label: 'Profile settings' });
  }

  // The tray's Display section, built from R's `gear_state`: Timeline as a
  // segmented control, then two checkboxes. The input values keep the wire
  // format the server always read (mode "rday"/"date", show_prestudy TRUE
  // for the full history, smooth "auto"/"off"). Without a treatment start
  // (rday false) there is no relative day and no day -30 to cut at, so
  // those two are left out rather than disabled.
  /** @param {{ rday: boolean, mode: string, prestudy: boolean, smooth: string }} st */
  function buildDisplay(st) {
    var host = byId(gearDisplayId);
    if (!host || !window.Blockr) return;
    host.textContent = '';

    var title = document.createElement('div');
    title.className = 'blockr-settings__title';
    title.textContent = 'Display';
    var grid = document.createElement('div');
    grid.className = 'blockr-settings__grid';

    /** @param {string} cls @param {HTMLElement[]} kids */
    var field = function(cls, kids) {
      var f = document.createElement('div');
      f.className = 'blockr-settings__field' + (cls ? ' ' + cls : '');
      kids.forEach(function(k) { f.appendChild(k); });
      grid.appendChild(f);
      return f;
    };

    if (st.rday) {
      var lbl = document.createElement('span');
      lbl.className = 'blockr-label';
      lbl.textContent = 'Timeline';
      var seg = Blockr.segmented(
        [{ value: 'date', label: 'Date' }, { value: 'rday', label: 'Relative day' }],
        st.mode === 'date' ? 'date' : 'rday',
        function(v) { Shiny.setInputValue(tlModeInputId, v, {priority: 'event'}); },
        { label: 'Timeline' }
      );
      field('pp-gear-timeline', [lbl, seg.el]);
      var clip = Blockr.checkbox('Hide data before day −30', !st.prestudy,
        function(on) {
          Shiny.setInputValue(prestudyInputId, !on, {priority: 'event'});
        });
      field('pp-gear-clip', [clip.el]);
    }
    var smooth = Blockr.checkbox('Smooth lines', st.smooth !== 'off',
      function(on) {
        Shiny.setInputValue(smoothInputId, on ? 'auto' : 'off', {priority: 'event'});
      });
    field('blockr-settings__field--small pp-gear-smooth', [smooth.el]);

    host.appendChild(title);
    host.appendChild(grid);
  }

  Shiny.addCustomMessageHandler(gearStateMsgId, function(msg) {
    if (msg) buildDisplay(msg);
  });
});
