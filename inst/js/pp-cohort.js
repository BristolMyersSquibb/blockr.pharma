// @ts-check
/* The cohort list: picking a patient by click or keyboard, the well's
 * height, the band each row draws as it scrolls into view, the sort clause,
 * and the server's word on who is selected.
 *
 * Depends on: pp-core.js
 */
PatientProfile.part(function(ctx) {
  var ns = ctx.ns;
  var cfg = ctx.cfg;
  var layoutId = ns('pp_layout');
  var pickSubjectInputId = ns('pick_subject');
  var cohortSortInputId = ns('cohort_sort');
  var cohortSortById = ns('cohort_sort_by');
  var cohortWellId = ns('pp_cohort_well');
  var syncSubjectMsgId = ns('sync_subject');

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

  // Every cohort re-render brings undrawn rows.
  $(document).on('shiny:value', function(e) {
    if (e.name && e.name.indexOf('sidebar_cohort') >= 0) {
      setTimeout(observeBands, 0);
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
    var idx = (parseInt($by.attr('data-index') || '0', 10) + 1) %
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
    if ($row.length) {
      $row[0].scrollIntoView({block: 'nearest'});
    }
  });
});
