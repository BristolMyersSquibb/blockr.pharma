/* The block header: the cohort count that opens the sidebar, the download
 * menu, the gear and its three toggles.
 *
 * Depends on: pp-core.js
 */
PatientProfile.part(function(ctx) {
  var ns = ctx.ns;
  var layoutId = ns('pp_layout');
  var sidebarId = ns('pp_sidebar');
  var tlModeInputId = ns('timeline_mode');
  var prestudyInputId = ns('show_prestudy');
  var smoothInputId = ns('smooth_mode');
  var gearBtnId = ns('pp_gear_btn');
  var gearPopoverId = ns('pp_gear_popover');
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
});
