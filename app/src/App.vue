<template>
  <div>
    <nav id="sidebar" class="sidebar" aria-label="Sidebar">
      <div class="sidebar-scrollbox"></div>
      <div id="sidebar-resize-handle" class="sidebar-resize-handle"></div>
    </nav>

    <div id="page-wrapper" class="page-wrapper">
      <div class="page">
        <div id="menu-bar-hover-placeholder"></div>
        <div id="menu-bar" class="menu-bar sticky bordered">
          <div class="left-buttons">
            <button
              id="sidebar-toggle"
              class="icon-button"
              type="button"
              title="Toggle Sidebar"
              aria-label="Toggle Sidebar"
              aria-controls="sidebar"
            >
              <i class="fa fa-bars"></i>
            </button>
          </div>

          <h1 class="menu-title">The WEAVE Processor</h1>
        </div>

        <div id="content" class="content">
          <main id="main">
            <!-- filled in dynamically! -->
          </main>
        </div>
      </div>

      <div id="pager-bar" class="pager-bar sticky bordered">
        <p>paging happening here!</p>
      </div>
    </div>

    <div class="modal-overlay" id="modal-overlay"></div>

    <div class="modal-wrapper">
      <div class="modal-container page-wrapper" id="contents-modal">
        <div class="content-aligned">
          <h1>Contents</h1>

          <div id="contents-modal-contents">
            <p><i>… contents loading …</i></p>
          </div>
        </div>
      </div>

      <div class="modal-container page-wrapper" id="goto-modal">
        <div class="content-aligned">
          <h1>Go To Module</h1>

          <div id="goto-modal-contents">
            <form id="goto-modal-form">
              <!-- This is a numeric entry, but UX-wise I think it's better to code it as text -->
              <div>
                <label for="goto-modal-entry">Enter a module number:</label>
              </div>
              <div>
                <input
                  type="text"
                  id="goto-modal-entry"
                  name="goto-modal-entry"
                />
              </div>
              <div>
                <p id="goto-modal-error" class="goto-modal-error"></p>
              </div>
              <div>
                <button>Go</button>
              </div>
            </form>
          </div>
        </div>
      </div>

      <div class="modal-container page-wrapper" id="modinfo-modal">
        <div class="content-aligned">
          <h1 id="modinfo-modal-title">Module Information</h1>

          <div id="modinfo-modal-contents">
            <p id="modinfo-modal-def-desc">This module is defined in...</p>
            <ul id="modinfo-modal-def-list" class="modinfo-reflist"></ul>
            <p id="modinfo-modal-ref-desc">This module is referenced in...</p>
            <ul id="modinfo-modal-ref-list" class="modinfo-reflist"></ul>
          </div>
        </div>
      </div>
    </div>
  </div>
</template>

<style src="./ttw-style.scss"></style>

<script>
export default {
  data() {
    return {
      name: "Vue",
    };
  },
};
</script>