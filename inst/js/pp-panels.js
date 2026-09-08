/* The panels: the ghost held over a chart while it re-renders, dragging a
 * panel by its header, the band tag, the controls in a panel header, the
 * find box and its restore, and keeping every chart the width of its
 * container.
 *
 * Depends on: pp-core.js
 */
PatientProfile.part(function(ctx) {
  var ns = ctx.ns;
  var layoutId = ns('pp_layout');
  var toggleInputId = ns('toggle_viz');
  var ctrlInputId = ns('viz_ctrl');
  var chartAreaId = ns('chart_area');
  var syncBandMsgId = ns('sync_band');
  var reorderInputId = ns('reorder_viz');

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

  $(document).on('shiny:value', function(e) {
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
  });

  // Panel x: remove the viz. Same input the picker sends, so the server
  // deselects it and its On row leaves the sidebar.
  $(document).on('click', '#' + layoutId + ' .pp-chart-remove',
    function(e) {
      e.stopPropagation();
      var vizId = $(this).attr('data-viz-id');
      if (!vizId) return;
      Shiny.setInputValue(toggleInputId, vizId, {priority: 'event'});
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
});
