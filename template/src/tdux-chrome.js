"use strict";

// Fix back button cache problem
window.onunload = function () { };

(function sidebar() {
  var html = document.querySelector("html");
  var sidebar = document.getElementById("sidebar");
  var sidebarLinks = document.querySelectorAll('#sidebar a');
  var sidebarToggleButton = document.getElementById("sidebar-toggle");
  var sidebarResizeHandle = document.getElementById("sidebar-resize-handle");
  var firstContact = null;

  function showSidebar() {
    html.classList.remove('sidebar-hidden')
    html.classList.add('sidebar-visible');
    Array.from(sidebarLinks).forEach(function (link) {
      link.setAttribute('tabIndex', 0);
    });
    sidebarToggleButton.setAttribute('aria-expanded', true);
    sidebar.setAttribute('aria-hidden', false);
    try { localStorage.setItem('tdux-sidebar', 'visible'); } catch (e) { }
  }

  var sidebarAnchorToggles = document.querySelectorAll('#sidebar a.toggle');

  function toggleSection(ev) {
    ev.currentTarget.parentElement.classList.toggle('expanded');
  }

  Array.from(sidebarAnchorToggles).forEach(function (el) {
    el.addEventListener('click', toggleSection);
  });

  function hideSidebar() {
    html.classList.remove('sidebar-visible')
    html.classList.add('sidebar-hidden');
    Array.from(sidebarLinks).forEach(function (link) {
      link.setAttribute('tabIndex', -1);
    });
    sidebarToggleButton.setAttribute('aria-expanded', false);
    sidebar.setAttribute('aria-hidden', true);
    try { localStorage.setItem('tdux-sidebar', 'hidden'); } catch (e) { }
  }

  // Toggle sidebar
  sidebarToggleButton.addEventListener('click', function sidebarToggle() {
    if (html.classList.contains("sidebar-hidden")) {
      var current_width = parseInt(
        document.documentElement.style.getPropertyValue('--sidebar-width'), 10);
      if (current_width < 150) {
        document.documentElement.style.setProperty('--sidebar-width', '150px');
      }
      showSidebar();
    } else if (html.classList.contains("sidebar-visible")) {
      hideSidebar();
    } else {
      if (getComputedStyle(sidebar)['transform'] === 'none') {
        hideSidebar();
      } else {
        showSidebar();
      }
    }
  });

  sidebarResizeHandle.addEventListener('mousedown', initResize, false);

  function initResize(e) {
    window.addEventListener('mousemove', resize, false);
    window.addEventListener('mouseup', stopResize, false);
    html.classList.add('sidebar-resizing');
  }

  function resize(e) {
    var pos = (e.clientX - sidebar.offsetLeft);
    if (pos < 20) {
      hideSidebar();
    } else {
      if (html.classList.contains("sidebar-hidden")) {
        showSidebar();
      }
      pos = Math.min(pos, window.innerWidth - 100);
      document.documentElement.style.setProperty('--sidebar-width', pos + 'px');
    }
  }

  //on mouseup remove windows functions mousemove & mouseup
  function stopResize(e) {
    html.classList.remove('sidebar-resizing');
    window.removeEventListener('mousemove', resize, false);
    window.removeEventListener('mouseup', stopResize, false);
  }

  document.addEventListener('touchstart', function (e) {
    firstContact = {
      x: e.touches[0].clientX,
      time: Date.now()
    };
  }, { passive: true });

  document.addEventListener('touchmove', function (e) {
    if (!firstContact)
      return;

    var curX = e.touches[0].clientX;
    var xDiff = curX - firstContact.x,
      tDiff = Date.now() - firstContact.time;

    if (tDiff < 250 && Math.abs(xDiff) >= 150) {
      if (xDiff >= 0 && firstContact.x < Math.min(document.body.clientWidth * 0.25, 300))
        showSidebar();
      else if (xDiff < 0 && curX < 300)
        hideSidebar();

      firstContact = null;
    }
  }, { passive: true });

  // Scroll sidebar to current active section
  var activeSection = document.getElementById("sidebar").querySelector(".active");
  if (activeSection) {
    // https://developer.mozilla.org/en-US/docs/Web/API/Element/scrollIntoView
    activeSection.scrollIntoView({ block: 'center' });
  }
})();

// Modals

var modals = (function modals() {
  var modals = {};

  var modalOverlay = document.getElementById("modal-overlay");

  function makeModal(elid) {
    var el = document.getElementById(elid);
    var state = {
      visible: false
    };

    state.open = function() {
      if (state.visible) {
        return;
      }

      for (const [_mname, mstate] of Object.entries(modals)) {
        if (mstate.visible) {
          mstate.close();
        }
      }

      modalOverlay.classList.add("modal-overlay-visible");
      el.classList.add("modal-container-visible");
      document.body.style.overflow = "hidden";
      state.visible = true;
    }

    state.close = function() {
      if (!state.visible) {
        return;
      }

      modalOverlay.classList.remove("modal-overlay-visible");
      el.classList.remove("modal-container-visible");
      document.querySelector("body").style.overflow = "visible";
      state.visible = false;
    }

    state.toggle = function() {
      if (state.visible) {
        state.close();
      } else {
        state.open();
      }
    }

    return state;
  }

  modals.contents = makeModal("contents-modal");
  modals.goto = makeModal("goto-modal");
  return modals;
})();

// Keyboard shortcuts

(function keyboard() {
  document.addEventListener("keypress", function onEvent(event) {
    if (event.key === "c") {
      modals.contents.toggle();
    } else if (event.key === "g") {
      modals.goto.toggle();
    }
  });
})();

// Populating the contents: the index of major modules.

(function majorModuleIndex() {
  function loaded() {
    var container = document.getElementById("contents-modal-contents");

    // Clear "loading" default contents, replace with list
    container.innerHTML = "<ul></ul>";
    var ul = container.firstChild;

    Array.from(ttWeaveMajorModuleIndex).forEach(function (info) {
      var id = info.id;
      var desc = info.d;

      var li = document.createElement('li');
      ul.appendChild(li);

      var a = document.createElement('a');
      li.appendChild(a);
      a.href = `#m${id}`;
      a.innerText = `${id}. ${desc}`;
      a.addEventListener("click", function () { modals.contents.toggle(); });
    });
  }

  // Synthesize the script element here so that we can be sure that our `load`
  // handler is installed in time.
  var script = document.createElement("script")
  script.async = true;
  script.addEventListener("load", loaded);
  script.src = "web-major-module-index.js";
  document.body.appendChild(script);
})();