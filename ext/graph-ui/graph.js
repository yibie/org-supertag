// org-supertag Graph UI
(function () {
  'use strict';

  // --- Config ---
  var WS_PORT = 35904;
  var RECONNECT_DELAY = 2000;

  // --- Tag color palette ---
  var TAG_COLORS = [
    '#4a90d9', '#e06c75', '#98c379', '#e5c07b', '#c678dd',
    '#56b6c2', '#d19a66', '#61afef', '#be5046', '#7ec699'
  ];
  var tagColorMap = {};
  var tagColorIdx = 0;

  function tagColor(tag) {
    if (!tag) return getComputedStyle(document.documentElement).getPropertyValue('--node-default').trim();
    if (!tagColorMap[tag]) {
      tagColorMap[tag] = TAG_COLORS[tagColorIdx % TAG_COLORS.length];
      tagColorIdx++;
    }
    return tagColorMap[tag];
  }

  // --- State ---
  var graphData = { nodes: [], links: [] };
  var ws = null;
  var graph = null;
  var searchTerm = '';
  var hiddenTypes = {};
  var showLabels = true;
  var showOrphans = true;
  var highlightNodes = new Set();
  var hoverNode = null;
  var linkDegree = {};  // node id -> { in, out }

  // --- DOM refs ---
  var statusEl = document.getElementById('status');
  var statsEl = document.getElementById('stats');
  var searchEl = document.getElementById('search');
  var filtersEl = document.getElementById('type-filters');
  var tooltipEl = document.getElementById('tooltip');
  var showLabelsEl = document.getElementById('show-labels');
  var showOrphansEl = document.getElementById('show-orphans');
  var themeBtn = document.getElementById('theme-toggle');

  // --- WebSocket ---
  function connect() {
    statusEl.textContent = 'Connecting...';
    try {
      ws = new WebSocket('ws://localhost:' + WS_PORT);
    } catch (e) {
      statusEl.textContent = 'Connection failed';
      setTimeout(connect, RECONNECT_DELAY);
      return;
    }

    ws.onopen = function () {
      statusEl.textContent = 'Connected';
      ws.send(JSON.stringify({ type: 'requestGraphData' }));
    };

    ws.onclose = function () {
      statusEl.textContent = 'Disconnected';
      setTimeout(connect, RECONNECT_DELAY);
    };

    ws.onerror = function () {
      statusEl.textContent = 'Error';
    };

    ws.onmessage = function (evt) {
      var msg;
      try { msg = JSON.parse(evt.data); } catch (e) { return; }

      if (msg.type === 'graphdata') {
        handleGraphData(msg.data);
      } else if (msg.type === 'focus') {
        handleFocus(msg.data);
      }
    };
  }

  // --- Graph Rendering ---
  function initGraph() {
    var container = document.getElementById('graph');
    graph = ForceGraph()(container)
      .graphData({ nodes: [], links: [] })
      .nodeId('id')
      .linkSource('source')
      .linkTarget('target')
      .nodeLabel('')  // we handle tooltips ourselves
      .nodeVal(function (n) { return nodeSize(n); })
      .nodeColor(function (n) { return nodeColor(n); })
      .nodeCanvasObjectMode(function () { return showLabels ? 'after' : undefined; })
      .nodeCanvasObject(function (node, ctx, globalScale) {
        if (!showLabels) return;
        var label = node.title || '';
        if (label.length > 30) label = label.substring(0, 27) + '...';
        var fontSize = Math.max(10 / globalScale, 2);
        // Only show labels for large/connected nodes when zoomed out
        if (globalScale < 0.6 && nodeSize(node) < 3) return;
        ctx.font = fontSize + 'px sans-serif';
        ctx.textAlign = 'center';
        ctx.textBaseline = 'top';
        ctx.fillStyle = isHighlighted(node) ? '#fff' : 'rgba(200,200,200,0.7)';
        ctx.fillText(label, node.x, node.y + nodeSize(node) + 2);
      })
      .linkColor(function (l) { return linkColor(l); })
      .linkWidth(function (l) { return isHighlightedLink(l) ? 2 : 0.5; })
      .linkLineDash(function (l) { return l.style === 'dashed' ? [4, 2] : null; })
      .linkDirectionalArrowLength(4)
      .linkDirectionalArrowRelPos(1)
      .onNodeClick(function (node) {
        if (ws && ws.readyState === WebSocket.OPEN) {
          ws.send(JSON.stringify({ type: 'open', data: { id: node.id } }));
        }
      })
      .onNodeHover(function (node) {
        hoverNode = node;
        if (node) {
          showTooltip(node);
        } else {
          tooltipEl.style.display = 'none';
        }
        document.getElementById('graph').style.cursor = node ? 'pointer' : 'default';
      })
      .onBackgroundClick(function () {
        highlightNodes.clear();
        graph.nodeColor(graph.nodeColor());  // force refresh
      })
      .d3Force('charge').strength(-80);

    graph.d3Force('link').distance(60);

    // Track mouse for tooltip positioning
    container.addEventListener('mousemove', function (e) {
      if (hoverNode) {
        tooltipEl.style.left = (e.clientX + 12) + 'px';
        tooltipEl.style.top = (e.clientY + 12) + 'px';
      }
    });
  }

  function handleGraphData(data) {
    graphData = data || { nodes: [], links: [] };
    computeDegrees();
    updateFilters();
    applyFilters();
    statsEl.textContent = graphData.nodes.length + ' nodes, ' + graphData.links.length + ' links';
  }

  function handleFocus(data) {
    if (!data || !data.id || !graph) return;
    var node = graphData.nodes.find(function (n) { return n.id === data.id; });
    if (node) {
      graph.centerAt(node.x, node.y, 500);
      graph.zoom(2, 500);
      highlightNodes.clear();
      highlightNodes.add(node.id);
      graph.nodeColor(graph.nodeColor());
    }
  }

  // --- Degree & Sizing ---
  function computeDegrees() {
    linkDegree = {};
    graphData.nodes.forEach(function (n) {
      linkDegree[n.id] = { in: 0, out: 0 };
    });
    graphData.links.forEach(function (l) {
      var src = typeof l.source === 'object' ? l.source.id : l.source;
      var tgt = typeof l.target === 'object' ? l.target.id : l.target;
      if (linkDegree[src]) linkDegree[src].out++;
      if (linkDegree[tgt]) linkDegree[tgt].in++;
    });
  }

  function nodeSize(node) {
    var d = linkDegree[node.id];
    var total = d ? d.in + d.out : 0;
    return 1 + Math.sqrt(total) * 1.5;
  }

  function nodeColor(node) {
    if (isHighlighted(node)) return '#fff';
    if (searchTerm && !matchesSearch(node)) return 'rgba(100,100,100,0.3)';
    var tags = node.tags;
    if (tags && tags.length > 0) return tagColor(tags[0]);
    return getComputedStyle(document.documentElement).getPropertyValue('--node-default').trim();
  }

  function linkColor(link) {
    if (link.color) return link.color;
    return getComputedStyle(document.documentElement).getPropertyValue('--link-default').trim();
  }

  function isHighlighted(node) {
    return highlightNodes.has(node.id);
  }

  function isHighlightedLink(link) {
    var src = typeof link.source === 'object' ? link.source.id : link.source;
    var tgt = typeof link.target === 'object' ? link.target.id : link.target;
    return highlightNodes.has(src) || highlightNodes.has(tgt);
  }

  function matchesSearch(node) {
    if (!searchTerm) return true;
    var term = searchTerm.toLowerCase();
    return (node.title && node.title.toLowerCase().indexOf(term) >= 0) ||
           (node.tags && node.tags.some(function (t) { return t.toLowerCase().indexOf(term) >= 0; }));
  }

  // --- Filtering ---
  function updateFilters() {
    var types = {};
    graphData.links.forEach(function (l) {
      if (l.type && !types[l.type]) {
        types[l.type] = { label: l.label || l.type, color: l.color || '' };
      }
    });

    filtersEl.innerHTML = '';
    Object.keys(types).sort().forEach(function (type) {
      var info = types[type];
      var label = document.createElement('label');
      var cb = document.createElement('input');
      cb.type = 'checkbox';
      cb.checked = !hiddenTypes[type];
      cb.dataset.type = type;
      cb.addEventListener('change', function () {
        if (this.checked) {
          delete hiddenTypes[type];
        } else {
          hiddenTypes[type] = true;
        }
        applyFilters();
      });
      var dot = document.createElement('span');
      dot.className = 'type-dot';
      dot.style.backgroundColor = info.color || 'var(--link-default)';
      label.appendChild(cb);
      label.appendChild(dot);
      label.appendChild(document.createTextNode(' ' + info.label));
      filtersEl.appendChild(label);
    });
  }

  function applyFilters() {
    if (!graph) return;

    // Filter links by type
    var filteredLinks = graphData.links.filter(function (l) {
      return !hiddenTypes[l.type];
    });

    // Determine connected nodes
    var connectedNodes = new Set();
    filteredLinks.forEach(function (l) {
      var src = typeof l.source === 'object' ? l.source.id : l.source;
      var tgt = typeof l.target === 'object' ? l.target.id : l.target;
      connectedNodes.add(src);
      connectedNodes.add(tgt);
    });

    // Filter nodes
    var filteredNodes = graphData.nodes.filter(function (n) {
      if (!showOrphans && !connectedNodes.has(n.id)) return false;
      return true;
    });

    graph.graphData({ nodes: filteredNodes, links: filteredLinks });
  }

  // --- Tooltip ---
  function showTooltip(node) {
    var d = linkDegree[node.id] || { in: 0, out: 0 };
    var tagsStr = (node.tags && node.tags.length > 0) ? node.tags.join(', ') : '';
    tooltipEl.innerHTML =
      '<div class="tt-title">' + escapeHtml(node.title || 'Untitled') + '</div>' +
      (tagsStr ? '<div class="tt-tags">#' + escapeHtml(tagsStr).replace(/, /g, ' #') + '</div>' : '') +
      '<div class="tt-stats">In: ' + d.in + ' | Out: ' + d.out + '</div>';
    tooltipEl.style.display = 'block';
  }

  function escapeHtml(s) {
    var div = document.createElement('div');
    div.appendChild(document.createTextNode(s));
    return div.innerHTML;
  }

  // --- Controls ---
  searchEl.addEventListener('input', function () {
    searchTerm = this.value;
    if (graph) graph.nodeColor(graph.nodeColor());
  });

  showLabelsEl.addEventListener('change', function () {
    showLabels = this.checked;
    if (graph) {
      graph.nodeCanvasObjectMode(function () { return showLabels ? 'after' : undefined; });
    }
  });

  showOrphansEl.addEventListener('change', function () {
    showOrphans = this.checked;
    applyFilters();
  });

  themeBtn.addEventListener('click', function () {
    document.body.classList.toggle('light');
  });

  // --- Init ---
  initGraph();
  connect();
})();
