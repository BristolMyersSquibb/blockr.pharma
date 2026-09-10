# Browser pins for the patient profile block.
#
# The block's client half (the JS in patient-profile-block.R) has behaviour
# that no R test can see: which row an arrow key lands on, whether a burst of
# keys sends one pick or twenty, when the ghost lifts, how the cohort list
# sizes itself. These tests pin what the block does today, in a headless
# chromium, so that moving the JS out of the R file and refactoring it can be
# proven to change nothing.
#
# ONE app (apps/pp-e2e) is launched at file load and shared, as in
# blockr.viz. Tests run in file order and each one leaves the app in the
# state it found it, except the last, which strips the profile down to the
# lab panel to reach the series band.
#
# Skipped where no headless browser exists, and under R CMD check.

library(shinytest2)

run_browser <-
  !nzchar(Sys.getenv("_R_CHECK_PACKAGE_NAME_")) &&
  requireNamespace("shinytest2", quietly = TRUE) &&
  requireNamespace("chromote", quietly = TRUE) &&
  chromote_works()

app <- NULL
if (run_browser) {
  Sys.setenv(NOT_CRAN = "true")
  configure_chromote()
  app <- tryCatch(
    AppDriver$new(
      test_path("apps", "pp-e2e"),
      name = "pp-e2e",
      width = 1600,
      height = 1100,
      load_timeout = 120 * 1000,
      timeout = 30 * 1000
    ),
    error = function(e) {
      message("pp-e2e app launch failed: ", conditionMessage(e))
      NULL
    }
  )
  if (!is.null(app)) {
    app$wait_for_idle()
    withr::defer(app$stop(), testthat::teardown_env())
  }
}

skip_if_no_app <- function() {
  testthat::skip_if(is.null(app), "pp-e2e browser app unavailable")
}

# ---------------------------------------------------------------------------
# Helpers: everything reads the DOM the way the user sees it.
# ---------------------------------------------------------------------------

js <- function(code) app$get_js(code)
run_js <- function(code) invisible(app$run_js(code))

wait_js <- function(code, timeout = 30000) {
  app$wait_for_js(code, timeout = timeout)
}

# The block's Shiny namespace, read off the layout rather than assumed.
pp_ns <- function() {
  js("document.querySelector('.pp-layout').id.replace(/-pp_layout$/, '')")
}

shown_ids <- function() {
  unlist(js("[...document.querySelectorAll('.pp-pt:not(.is-filtered-out)')]
              .map(r => r.getAttribute('data-usubjid'))"))
}

selected_id <- function() {
  js("(function(){ var r = document.querySelector('.pp-pt.is-selected');
       return r ? r.getAttribute('data-usubjid') : null; })()")
}

header_who <- function() {
  js("(function(){ var e = document.querySelector('.pp-subject-who');
       return e ? e.innerText.trim() : null; })()")
}

on_profile <- function() {
  unlist(js("[...document.querySelectorAll('.pp-add-ord')]
              .map(r => r.getAttribute('data-viz-id'))"))
}

slot_ids <- function() {
  unlist(js("[...document.querySelectorAll('[id*=viz_slot_]')]
              .map(e => e.id.split('viz_slot_')[1])"))
}

search_value <- function() {
  js("document.querySelector('input[id$=\"-search\"]').value")
}

# The profile is showing `id`: header names it, no ghost is left, and every
# panel has painted a canvas.
wait_profile <- function(id) {
  wait_js(sprintf(
    "(function(){
       var w = document.querySelector('.pp-subject-who');
       return !!w && w.innerText.trim() === '%s' &&
         !document.querySelector('.pp-chart-ghost') &&
         document.querySelectorAll('[id*=viz_slot_] canvas').length > 0;
     })()", id))
  app$wait_for_idle()
}

pick_patient <- function(id) {
  run_js(sprintf(
    "document.querySelector('.pp-pt[data-usubjid=\"%s\"]').click();", id))
  wait_profile(id)
}

# A spy on what the client sends and what the server renders, installed
# once; spy_reset() clears it before a measured gesture.
spy_install <- function() {
  run_js("
    if (!window.__pp) {
      window.__pp = {inputs: [], values: {}};
      var orig = Shiny.setInputValue;
      Shiny.setInputValue = function(name, value, opts) {
        window.__pp.inputs.push({name: name, value: value});
        return orig.apply(this, arguments);
      };
      $(document).on('shiny:value', function(e) {
        window.__pp.values[e.name] = (window.__pp.values[e.name] || 0) + 1;
      });
    }")
}

spy_reset <- function() run_js("window.__pp.inputs = []; window.__pp.values = {};")

spy_inputs <- function(suffix) {
  out <- js(sprintf(
    "window.__pp.inputs.filter(i => i.name.endsWith('%s')).map(i => i.value)",
    suffix))
  if (is.null(out)) list() else out
}

spy_values <- function() {
  out <- js("window.__pp.values")
  if (is.null(out)) list() else out
}

# Real key and text input through CDP, so the block's own handlers see the
# same events a keyboard produces.
vk <- c(ArrowDown = 40, ArrowUp = 38, Enter = 13, Escape = 27,
        Home = 36, End = 35)

cdp_key <- function(key) {
  s <- app$get_chromote_session()
  s$Input$dispatchKeyEvent(type = "keyDown", key = key, code = key,
                           windowsVirtualKeyCode = vk[[key]])
  s$Input$dispatchKeyEvent(type = "keyUp", key = key, code = key,
                           windowsVirtualKeyCode = vk[[key]])
  invisible()
}

search_type <- function(text) {
  run_js("document.querySelector('input[id$=\"-search\"]').focus();")
  app$get_chromote_session()$Input$insertText(text = text)
  Sys.sleep(0.3)
}

search_clear <- function() {
  run_js("(function(){
    var el = document.querySelector('input[id$=\"-search\"]');
    el.value = ''; el.dispatchEvent(new Event('input', {bubbles: true}));
  })()")
  Sys.sleep(0.3)
}

# A left drag from one viewport point to another through CDP.
cdp_drag <- function(x0, y0, x1, y1) {
  s <- app$get_chromote_session()
  s$Input$dispatchMouseEvent(type = "mousePressed", x = x0, y = y0,
                             button = "left", buttons = 1, clickCount = 1)
  steps <- 6
  for (i in seq_len(steps)) {
    s$Input$dispatchMouseEvent(type = "mouseMoved",
                               x = x0 + (x1 - x0) * i / steps,
                               y = y0 + (y1 - y0) * i / steps,
                               button = "left", buttons = 1)
    Sys.sleep(0.03)
  }
  s$Input$dispatchMouseEvent(type = "mouseReleased", x = x1, y = y1,
                             button = "left", buttons = 1, clickCount = 1)
  invisible()
}

rect_of <- function(selector) {
  js(sprintf(
    "(function(){ var e = document.querySelector('%s');
       if (!e) return null; var r = e.getBoundingClientRect();
       return {top: r.top, bottom: r.bottom, left: r.left, right: r.right,
               width: r.width, height: r.height}; })()", selector))
}

# ---------------------------------------------------------------------------
# 1. Boot: the cohort renders, a click picks a patient.
# ---------------------------------------------------------------------------

test_that("the cohort lists every subject and a click picks one", {
  skip_if_no_app()

  wait_js("document.querySelectorAll('.pp-pt').length >= 250", 120000)
  app$wait_for_idle()
  spy_install()

  ids <- shown_ids()
  expect_length(ids, 254)
  expect_null(selected_id())
  expect_equal(js("document.querySelectorAll('[id*=viz_slot_] canvas').length"), 0)
  # Before any pick the chart area explains itself, and nothing in the
  # block has errored.
  expect_true(js("!!document.querySelector('.pp-chart-area .pp-empty-state')"))
  expect_match(js("document.querySelector('.pp-empty-state-hint').innerText"), "Pick one of 254")
  expect_equal(js("document.querySelectorAll('.pp-layout .shiny-output-error').length"), 0)

  pick_patient(ids[1])
  expect_identical(selected_id(), ids[1])
  expect_identical(header_who(), ids[1])
  expect_length(slot_ids(), 5)
  expect_equal(js("document.querySelectorAll('.pp-layout .shiny-output-error').length"), 0)

  # The + Add button is gone: panels are picked in the sidebar.
  expect_true(js("document.querySelector('.pp-add-btn') === null"))
})

# ---------------------------------------------------------------------------
# 2. One patient click renders each panel exactly once.
# ---------------------------------------------------------------------------

test_that("a second pick updates the panels in place: no slot render, same canvases", {
  skip_if_no_app()

  ids <- shown_ids()
  # Mark every canvas, so a rebuilt one would show up unmarked.
  run_js("document.querySelectorAll('[id*=viz_slot_] canvas').forEach(c => { c.__kept = 1; });")
  spy_reset()
  pick_patient(ids[5])
  Sys.sleep(1)

  values <- spy_values()
  expect_false(any(grepl("viz_slot_", names(values))), "the slots were not re-rendered")
  expect_equal(values[[grep("subject_facts$", names(values))]], 1L)
  expect_false(any(grepl("chart_area$", names(values))))
  expect_true(js("[...document.querySelectorAll('[id*=viz_slot_] canvas')].every(c => c.__kept === 1)"))
  expect_gte(js("document.querySelectorAll('[id*=viz_slot_] canvas').length"), 5)
  # And the charts show the new patient: the treatment strip names its arm
  # from this patient's data, and the header reads the id.
  expect_identical(header_who(), ids[5])

  pick_patient(ids[1])
})

# ---------------------------------------------------------------------------
# 3. Arrow keys walk the rows in the order shown, and a burst sends one pick.
# ---------------------------------------------------------------------------

test_that("arrow keys walk the list in DOM order without a focus ring", {
  skip_if_no_app()

  ids <- shown_ids()
  pick_patient(ids[1])
  spy_reset()

  # The burst is timed inside the page: a key repeat is 25ms apart, and a
  # round trip from R per key cannot promise to stay under the 250ms the
  # block waits before it sends. The single presses further down go through
  # CDP, which is what proves the keys reach the list at all.
  run_js("(function(){
    var well = document.activeElement;
    window.__burst = false;
    var n = 0, t = setInterval(function() {
      well.dispatchEvent(new KeyboardEvent('keydown',
        {key: 'ArrowDown', code: 'ArrowDown', keyCode: 40, bubbles: true}));
      if (++n === 20) { clearInterval(t); window.__burst = true; }
    }, 25);
  })()")
  wait_js("window.__burst === true", 5000)
  # The moment the keys stop the selection is already 20 rows down.
  expect_identical(selected_id(), ids[21])
  expect_true(js("(function(){
    var cs = getComputedStyle(document.activeElement);
    return cs.outlineStyle === 'none' || cs.outlineWidth === '0px'; })()"))

  wait_profile(ids[21])
  Sys.sleep(1)
  # No jump back once the server answers, and the burst was one pick.
  expect_identical(selected_id(), ids[21])
  expect_identical(header_who(), ids[21])
  expect_identical(unlist(spy_inputs("-pick_subject")), ids[21])

  # Slow taps each load a patient.
  spy_reset()
  for (i in 1:3) {
    cdp_key("ArrowUp")
    Sys.sleep(1.2)
  }
  wait_profile(ids[18])
  expect_identical(unlist(spy_inputs("-pick_subject")), ids[20:18])

  # Home and End jump to the ends.
  cdp_key("End")
  expect_identical(selected_id(), ids[254])
  cdp_key("Home")
  expect_identical(selected_id(), ids[1])
  wait_profile(ids[1])
})

# ---------------------------------------------------------------------------
# 4. Picking a panel moves its row onto the profile and resets the search.
# ---------------------------------------------------------------------------

test_that("a picked panel moves to the On list and the search resets", {
  skip_if_no_app()

  search_clear()
  # A lab panel is one On row per parameter, so three panels are five rows.
  before <- on_profile()
  n0 <- length(before)
  expect_length(before, 5)
  expect_identical(js("document.querySelector('.pp-add-n').innerText.trim()"),
                   as.character(n0))
  slots0 <- length(slot_ids())

  search_type("TEMP")
  wait_js("[...document.querySelectorAll('.pp-add-row[data-viz-id*=\"TEMP\"]')]
             .some(r => r.offsetParent)")
  # The patient list is filtered down by the same box.
  expect_lt(length(shown_ids()), 254)

  run_js("[...document.querySelectorAll('.pp-add-row[data-viz-id*=\"TEMP\"]')]
            .filter(r => r.offsetParent)[0].click();")
  wait_js(sprintf("document.querySelectorAll('.pp-add-ord').length === %d", n0 + 1))
  wait_js("document.querySelector('input[id$=\"-search\"]').value === ''")
  app$wait_for_idle()

  after <- on_profile()
  expect_length(after, n0 + 1)
  expect_true(any(grepl("TEMP", after)))
  expect_identical(after[seq_len(n0)], before)
  expect_identical(js("document.querySelector('.pp-add-n').innerText.trim()"),
                   as.character(n0 + 1))
  expect_length(shown_ids(), 254)
  wait_js(sprintf("document.querySelectorAll('[id*=viz_slot_]').length === %d", slots0 + 1))

  # Remove it again: the row leaves the On list.
  run_js("document.querySelector('.pp-add-ord[data-viz-id*=\"TEMP\"] .pp-add-ord-x').click();")
  wait_js(sprintf("document.querySelectorAll('.pp-add-ord').length === %d", n0))
  app$wait_for_idle()
  expect_identical(on_profile(), before)
  wait_js(sprintf("document.querySelectorAll('[id*=viz_slot_]').length === %d", slots0))
})

# ---------------------------------------------------------------------------
# 5. Enter picks the first hit, panel before patient; Escape clears.
# ---------------------------------------------------------------------------

test_that("Enter picks the first hit and Escape clears the search", {
  skip_if_no_app()

  ids <- shown_ids()
  pick_patient(ids[1])
  target <- ids[grepl("1130", ids, fixed = TRUE)][1]
  expect_false(is.na(target))
  n0 <- length(on_profile())

  search_clear()
  search_type("1130")
  wait_js("!!document.querySelector('.pp-pt.is-enter')")
  expect_identical(
    js("document.querySelector('.is-enter').getAttribute('data-usubjid')"),
    target)
  cdp_key("Enter")
  wait_profile(target)
  expect_identical(selected_id(), target)
  # Only a PANEL pick resets the box; a patient pick keeps the query.
  expect_identical(search_value(), "1130")

  search_clear()
  search_type("TEMP")
  wait_js("!!document.querySelector('.pp-add-row.is-enter')")
  cdp_key("Enter")
  wait_js(sprintf("document.querySelectorAll('.pp-add-ord').length === %d", n0 + 1))
  expect_true(any(grepl("TEMP", on_profile())))
  expect_identical(search_value(), "")

  search_type("zzzz")
  wait_js("!!document.querySelector('.pp-add-none.is-shown')")
  expect_true(js("document.querySelector('.is-enter') === null"))
  expect_length(shown_ids(), 0)

  cdp_key("Escape")
  wait_js("document.querySelector('input[id$=\"-search\"]').value === ''")
  expect_length(shown_ids(), 254)
  expect_false(js("!!document.querySelector('.pp-add-none.is-shown')"))

  run_js("document.querySelector('.pp-add-ord[data-viz-id*=\"TEMP\"] .pp-add-ord-x').click();")
  wait_js(sprintf("document.querySelectorAll('.pp-add-ord').length === %d", n0))
  app$wait_for_idle()
  pick_patient(ids[1])
})

# ---------------------------------------------------------------------------
# 6. The cohort list sizes itself to the space it has.
# ---------------------------------------------------------------------------

test_that("the cohort well fits its host when the host shrinks", {
  skip_if_no_app()

  fit <- function() {
    js("(function(){
      var well = document.querySelector('.pp-cohort-well');
      var n = well.parentElement, host = null;
      while (n && n !== document.body) {
        var cs = getComputedStyle(n);
        if (cs.overflowY === 'auto' || cs.overflowY === 'scroll') { host = n; break; }
        n = n.parentElement;
      }
      var w = well.getBoundingClientRect();
      var limit = host ? host.getBoundingClientRect().bottom : window.innerHeight;
      return {maxH: parseFloat(well.style.maxHeight) || null,
              bottom: w.bottom, limit: limit, height: w.height,
              inner: window.innerHeight, host: host ? host.className : null};
    })()")
  }

  # The first scrolling ancestor is what the well measures against: on this
  # board that is the block's card body, in a dock it is the panel.
  host_js <- "(function(){
    var n = document.querySelector('.pp-cohort-well').parentElement;
    while (n && n !== document.body) {
      var cs = getComputedStyle(n);
      if (cs.overflowY === 'auto' || cs.overflowY === 'scroll') return n;
      n = n.parentElement;
    }
    return null; })()"

  tall <- fit()
  expect_false(is.null(tall$host))
  expect_false(is.null(tall$maxH))
  expect_lte(tall$bottom, tall$limit)

  # Shrink the host, as a dock panel does when the user drags a divider.
  run_js(sprintf("%s.style.height = '420px';", host_js))
  Sys.sleep(0.8)
  short <- fit()
  expect_lt(short$maxH, tall$maxH)
  expect_lte(short$bottom, short$limit)
  expect_gte(short$height, 132)

  run_js(sprintf("%s.style.height = '';", host_js))
  Sys.sleep(0.8)
  back <- fit()
  expect_equal(back$maxH, tall$maxH, tolerance = 2)
})

# ---------------------------------------------------------------------------
# 7. A narrow block does not clip the picker.
# ---------------------------------------------------------------------------

test_that("the panel picker stays inside the sidebar in a narrow block", {
  skip_if_no_app()

  search_clear()
  app$set_window_size(560, 900)
  Sys.sleep(0.8)
  search_type("TEMP")
  wait_js("[...document.querySelectorAll('.pp-add-row[data-viz-id*=\"TEMP\"]')]
             .some(r => r.offsetParent)")

  side <- rect_of(".pp-sidebar")
  panels <- rect_of(".pp-panels")
  expect_lte(panels$right, side$right + 1)
  expect_gte(panels$left, side$left - 1)
  expect_lte(side$right, 560)
  expect_gt(rect_of("input[id$=\"-search\"]")$width, 40)

  search_clear()
  app$set_window_size(1600, 1100)
  Sys.sleep(0.8)
  expect_length(shown_ids(), 254)
})

# ---------------------------------------------------------------------------
# 8. The ghost stays until the new charts have painted.
# ---------------------------------------------------------------------------

test_that("switching patients raises no ghost: nothing is torn down", {
  skip_if_no_app()

  ids <- shown_ids()
  pick_patient(ids[1])
  run_js("
    window.__ghost = []; var t0 = performance.now();
    var owner = new WeakMap();
    var mark = function(w, dt) { window.__ghost.push({t: performance.now() - t0, w: w, dt: dt || 0}); };
    var slots = function() { return [...document.querySelectorAll('[id*=viz_slot_]')]; };
    // The block drops every ghost on any scroll (a fixed overlay would hang
    // over the wrong content), so a scroll is a legitimate early exit. The
    // times are kept for R to match up: the observer's callback runs as a
    // microtask right after the block's own scroll listener, before this one.
    window.__scrolls = [];
    window.addEventListener('scroll', function() { window.__scrolls.push(performance.now() - t0); }, true);
    new MutationObserver(function(ms) { ms.forEach(function(m) {
      m.addedNodes.forEach(function(n) {
        if (n.classList && n.classList.contains('pp-chart-ghost')) {
          // The ghost sits on <body>; the panel it covers points at it.
          var slot = slots().find(function(s) { return s.__ghost === n; });
          if (slot) owner.set(n, {slot: slot, t: performance.now()});
          mark('ghost+');
        }
        if (n.tagName === 'CANVAS') mark('canvas');
      });
      m.removedNodes.forEach(function(n) {
        if (n.classList && n.classList.contains('pp-chart-ghost')) {
          var o = owner.get(n);
          var painted = !!(o && o.slot.querySelector('canvas'));
          var why = painted ? 'ghost-' :
            (o && !o.slot.querySelector('.html-widget')) ? 'ghost-nowidget' : 'ghost-early';
          mark(why, o ? performance.now() - o.t : -1);
        }
      });
    }); }).observe(document.body, {subtree: true, childList: true});")

  pick_patient(ids[7])
  Sys.sleep(1)
  log <- js("window.__ghost")
  what <- vapply(log, `[[`, "", "w")
  when <- vapply(log, `[[`, 0, "t")
  held <- vapply(log, `[[`, 0, "dt")

  trace <- paste(sprintf("%s@%.0f(+%.0f)", what, when, held), collapse = " ")
  # The panels are updated in place: no recalculating slot, no ghost, no
  # new canvas.
  expect_equal(sum(what == "ghost+"), 0, info = trace)
  expect_equal(sum(what == "canvas"), 0, info = trace)

  pick_patient(ids[1])
})

# ---------------------------------------------------------------------------
# 9. The sidebar collapses on the count and comes back.
# ---------------------------------------------------------------------------

test_that("the cohort count toggles the sidebar", {
  skip_if_no_app()

  cls <- function() js("document.querySelector('.pp-sidebar').className")
  expect_false(grepl("collapsed", cls()))

  run_js("document.querySelector('.pp-cohort-count').click();")
  Sys.sleep(0.5)
  expect_true(grepl("collapsed", cls()))
  expect_true(js("document.querySelector('.pp-layout').classList.contains('sidebar-collapsed')"))

  run_js("document.querySelector('.pp-cohort-count').click();")
  Sys.sleep(0.5)
  expect_false(grepl("collapsed", cls()))
  expect_false(js("document.querySelector('.pp-layout').classList.contains('sidebar-collapsed')"))
})

# ---------------------------------------------------------------------------
# 10. Sorting: the clause names the key and the rows reorder.
# ---------------------------------------------------------------------------

test_that("the sort clause cycles and reorders the cohort", {
  skip_if_no_app()

  search_clear()
  sort_text <- function() js("document.querySelector('.pp-cohort-sortby').innerText.trim()")
  ids0 <- shown_ids()
  expect_length(ids0, 254)
  t0 <- sort_text()
  expect_match(t0, "patient id")

  run_js("document.querySelector('.pp-cohort-sortby').click();")
  wait_js(sprintf("document.querySelector('.pp-cohort-sortby').innerText.trim() !== '%s'", t0))
  app$wait_for_idle()
  t1 <- sort_text()
  expect_false(identical(shown_ids(), ids0))
  expect_gt(js("document.querySelectorAll('.pp-pt .pp-pt-val').length"), 200)

  # Cycle back round to the id order.
  for (i in 1:3) {
    if (identical(sort_text(), t0)) break
    prev <- sort_text()
    run_js("document.querySelector('.pp-cohort-sortby').click();")
    wait_js(sprintf("document.querySelector('.pp-cohort-sortby').innerText.trim() !== '%s'", prev))
    app$wait_for_idle()
  }
  expect_identical(sort_text(), t0)
  expect_identical(shown_ids(), ids0)
})

# ---------------------------------------------------------------------------
# 11. Dragging a row in the On list reorders the panels.
# ---------------------------------------------------------------------------

test_that("dragging an On row reorders the profile", {
  skip_if_no_app()

  search_clear()
  before <- on_profile()
  expect_identical(slot_ids(), before)
  spy_reset()

  # The board's sticky toolbar floats over the top of the sidebar, so the
  # list is scrolled clear of it before the mouse goes down.
  run_js("document.querySelector('.pp-add-on').scrollIntoView({block: 'center'});")
  Sys.sleep(0.3)
  expect_match(js(sprintf(
    "(function(){ var r = document.querySelector('.pp-add-ord');
       var b = r.getBoundingClientRect();
       var e = document.elementFromPoint(b.left + 10, b.top + b.height / 2);
       return e ? e.closest('.pp-add-ord') ? 'row' : e.className : 'none'; })()")),
    "^row$")

  # First row dropped below the last: [a b c d e] becomes [b c d e a].
  r0 <- rect_of(".pp-add-ord:first-child")
  rl <- rect_of(".pp-add-ord:last-child")
  cdp_drag(r0$left + 10, r0$top + r0$height / 2,
           r0$left + 10, rl$bottom - 2)

  want <- c(before[-1], before[1])
  expect_identical(on_profile(), want)
  expect_identical(unlist(spy_inputs("-reorder_viz")), want)
  wait_js(sprintf(
    "[...document.querySelectorAll('[id*=viz_slot_]')].map(e => e.id.split('viz_slot_')[1]).join() === '%s'",
    paste(want, collapse = ",")))

  # And back: the last row dropped above the first.
  r0 <- rect_of(".pp-add-ord:last-child")
  r1 <- rect_of(".pp-add-ord:first-child")
  cdp_drag(r0$left + 10, r0$top + r0$height / 2,
           r0$left + 10, r1$top + 2)
  expect_identical(on_profile(), before)
  wait_js(sprintf(
    "[...document.querySelectorAll('[id*=viz_slot_]')].map(e => e.id.split('viz_slot_')[1]).join() === '%s'",
    paste(before, collapse = ",")))
})

# ---------------------------------------------------------------------------
# 12. The find control: ticking terms in the popover filters the panel.
# ---------------------------------------------------------------------------

test_that("the find popover filters the panel and the cohort strip", {
  skip_if_no_app()

  # A patient with adverse events to filter.
  pick_patient(js("document.querySelector('.pp-pt').getAttribute('data-usubjid')"))
  lanes <- function() {
    js("(function(){
          var el = document.querySelector('[id*=viz_slot_ae_gantt] .echarts4r');
          var i = el && echarts.getInstanceByDom(el);
          var y = i && i.getOption().yAxis;
          return (y && y[0] && y[0].data) ? y[0].data.length : 0;
        })()")
  }
  wait_js("document.querySelector('[id*=viz_slot_ae_gantt] canvas') !== null")
  before <- lanes()
  expect_gt(before, 1)

  # The header carries the options, so the popover has its list without a
  # round trip: it is drawn before anything reaches the server.
  spy_install(); spy_reset()
  run_js("document.querySelector('[id*=viz_slot_ae_gantt] .pp-ctrl-find').click();")
  wait_js("document.querySelector('.pp-find-pop.is-open') !== null")
  expect_gt(js("document.querySelectorAll('.pp-find-opt').length"), 1)
  expect_length(spy_inputs("-viz_ctrl"), 0)

  # The popover is parented to <body>, not to the panel it belongs to: the
  # header is replaced on every settings change and would take it with it.
  expect_identical(
    js("document.querySelector('.pp-find-pop').parentElement.tagName"), "BODY")

  # Tick the first body system. Nothing is sent yet: one change here redraws
  # the panel AND re-derives 254 cohort bands.
  term <- js("document.querySelector('.pp-find-opt .pp-find-text').innerText.trim()")
  run_js("document.querySelector('.pp-find-opt').click();")
  wait_js("document.querySelectorAll('.pp-find-tag').length === 1")
  expect_length(spy_inputs("-viz_ctrl"), 0)

  # Closing applies it, once.
  run_js("document.querySelector('.pp-find-done').click();")
  wait_js("document.querySelector('.pp-find-pop.is-open') === null")
  app$wait_for_idle()
  sent <- spy_inputs("-viz_ctrl")
  expect_length(sent, 1)
  expect_identical(sent[[1]]$param, "find")

  # The panel is narrower, the trigger says how many filters are on, and the
  # cohort caption echoes the term so the sparse bands are explained.
  wait_js(sprintf(
    "(function(){
       var el = document.querySelector('[id*=viz_slot_ae_gantt] .echarts4r');
       var i = el && echarts.getInstanceByDom(el);
       var y = i && i.getOption().yAxis;
       return y && y[0] && y[0].data && y[0].data.length < %d;
     })()", before))
  expect_identical(
    js("document.querySelector('.pp-ctrl-find-badge').innerText.trim()"), "1")
  expect_true(js("document.querySelector('.pp-cohort-bandcap-find') !== null"))
  expect_match(
    js("document.querySelector('.pp-cohort-bandcap-find').innerText"),
    substr(term, 1L, 8L), fixed = TRUE
  )

  # The caption chip is the way back out, and it clears every pick.
  run_js("document.querySelector('.pp-cohort-bandcap-find').click();")
  wait_js("document.querySelector('.pp-cohort-bandcap-find') === null")
  app$wait_for_idle()
  expect_equal(lanes(), before)
  expect_true(js("document.querySelector('.pp-ctrl-find-badge') === null"))
})

test_that("typing reaches terms the cohort has and this patient does not", {
  skip_if_no_app()
  skip_if_not_installed("safetyData")

  # A term this patient has none of, and a query that reaches it without
  # reaching anything they DO have -- so a hit under the split can only have
  # come from the cohort's vocabulary.
  adae <- safetyData::adam_adae
  who <- selected_id()
  mine <- unique(as.character(adae$AEDECOD[adae$USUBJID == who]))
  others <- setdiff(unique(as.character(adae$AEDECOD)), mine)
  q <- NULL
  for (term in others) {
    cand <- tolower(substr(term, 1L, 6L))
    if (nchar(cand) < 4L) next
    if (!any(grepl(cand, tolower(mine), fixed = TRUE))) { q <- cand; break }
  }
  expect_false(is.null(q))

  spy_install(); spy_reset()
  run_js("document.querySelector('[id*=viz_slot_ae_gantt] .pp-ctrl-find').click();")
  wait_js("document.querySelector('.pp-find-pop.is-open') !== null")
  # Opening asks for nothing: the cohort's vocabulary is about 16kB and is
  # only wanted by a reader who is looking past this patient.
  expect_length(spy_inputs("-find_vocab"), 0)

  run_js(sprintf("(function(){
      var el = document.querySelector('.pp-find-input');
      el.value = '%s';
      el.dispatchEvent(new Event('input', {bubbles: true}));
    })()", q))
  wait_js("document.querySelectorAll('.pp-find-opt.is-elsewhere').length > 0")

  # One request, and the rows are counted in PATIENTS: for a term this
  # patient has none of, the useful number is how much of the cohort does.
  expect_length(spy_inputs("-find_vocab"), 1)
  expect_match(
    js("document.querySelector('.pp-find-opt.is-elsewhere .pp-find-n').innerText"),
    "patient"
  )
  expect_true(js("document.querySelector('.pp-find-split') !== null"))

  # Picking one applies like any other pick, and the panel says plainly that
  # this patient has none of it rather than going blank.
  run_js("document.querySelector('.pp-find-opt.is-elsewhere').click();")
  run_js("document.querySelector('.pp-find-done').click();")
  app$wait_for_idle()
  wait_js("document.querySelector('.pp-ctrl-find-badge') !== null")
  expect_match(
    js("(function(){
          var el = document.querySelector('[id*=viz_slot_ae_gantt] .echarts4r');
          var i = echarts.getInstanceByDom(el);
          var t = i.getOption().title;
          return (t && t[0] && t[0].text) || '';
        })()"),
    "None of this patient"
  )

  # A second search session asks again, with the token it already holds, and
  # is told the list has not moved.
  run_js("document.querySelector('[id*=viz_slot_ae_gantt] .pp-ctrl-find-clear').click();")
  app$wait_for_idle()
  spy_reset()
  run_js("document.querySelector('[id*=viz_slot_ae_gantt] .pp-ctrl-find').click();")
  wait_js("document.querySelector('.pp-find-pop.is-open') !== null")
  run_js(sprintf("(function(){
      var el = document.querySelector('.pp-find-input');
      el.value = '%s';
      el.dispatchEvent(new Event('input', {bubbles: true}));
    })()", q))
  wait_js("document.querySelectorAll('.pp-find-opt.is-elsewhere').length > 0")
  sent <- spy_inputs("-find_vocab")
  expect_length(sent, 1)
  expect_true(nzchar(sent[[1]]$have))
  run_js("document.querySelector('.pp-find-done').click();")
  app$wait_for_idle()
})

# ---------------------------------------------------------------------------
# 13. The series band: min to max scale, reference range, sort by peak.
# ---------------------------------------------------------------------------

test_that("a lab band draws the reference range and sorts by peak", {
  skip_if_no_app()

  # Strip the two event panels so the lab is what the strip draws.
  slots0 <- length(slot_ids())
  for (n in slots0 - 1:2) {
    run_js("document.querySelector('.pp-chart-remove').click();")
    wait_js(sprintf("document.querySelectorAll('[id*=viz_slot_]').length === %d", n))
    app$wait_for_idle()
  }
  wait_js("document.querySelector('.pp-pt').hasAttribute('data-limit-lo')")
  Sys.sleep(0.5)
  # Rows draw their band as they scroll into view; walk the whole list once.
  for (i in 1:60) {
    at_end <- js("(function(){ var w = document.querySelector('.pp-cohort-well');
              w.scrollTop += Math.floor(w.clientHeight / 2);
              return w.scrollTop + w.clientHeight >= w.scrollHeight - 1; })()")
    Sys.sleep(0.2)
    if (isTRUE(at_end)) break
  }
  Sys.sleep(0.4)
  run_js("document.querySelector('.pp-cohort-well').scrollTop = 0;")
  Sys.sleep(0.4)

  band <- js("(function(){
    var row = document.querySelector('.pp-pt');
    var svg = row.querySelector('.pp-pt-band');
    var rects = [...svg.querySelectorAll('rect')];
    var spans = [...document.querySelectorAll('.pp-pt-band path')]
      .map(p => p.getBBox().height).sort((a, b) => a - b);
    // A multi-visit row draws a path, a single visit a dot.
    var rows = [...document.querySelectorAll('.pp-pt[data-band-kind=series]')];
    var withPath = rows.filter(r => (r.getAttribute('data-band') || '').length > 2 &&
                                    !r.getAttribute('data-dot')).length;
    var withDot = rows.filter(r => !!r.getAttribute('data-dot')).length;
    var undrawn = rows.filter(r => !r.getAttribute('data-band-drawn')).length;
    var dots = document.querySelectorAll('.pp-pt-band circle').length;
    return {h: +svg.getAttribute('height'), withPath: withPath, withDot: withDot,
            undrawn: undrawn, dots: dots,
            hi: +row.getAttribute('data-limit'), lo: +row.getAttribute('data-limit-lo'),
            rects: rects.map(r => ({y: +r.getAttribute('y'), h: +r.getAttribute('height'),
                                    fill: r.getAttribute('fill')})),
            n: spans.length, median: spans[Math.floor(spans.length / 2)]};
  })()")
  expect_equal(band$h, 30)
  expect_lt(band$hi, band$lo)
  expect_length(band$rects, 1)
  expect_match(band$rects[[1]]$fill, "pp-cohort-ref")
  expect_equal(band$rects[[1]]$y, band$hi)
  expect_equal(band$rects[[1]]$h, band$lo - band$hi, tolerance = 0.01)
  expect_equal(band$undrawn, 0)
  expect_gt(band$withPath, 150)
  expect_equal(band$n, band$withPath)
  expect_equal(band$dots, band$withDot)
  expect_gt(band$median, 3)
  expect_lte(band$median, 30)

  run_js("document.querySelector('.pp-cohort-sortby').click();")
  wait_js("document.querySelector('.pp-cohort-sortby').innerText.indexOf('peak') >= 0")
  app$wait_for_idle()
  expect_gt(js("document.querySelectorAll('.pp-pt .pp-pt-val').length"), 200)
})
