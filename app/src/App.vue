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
              <FontAwesomeIcon icon="fa-solid fa-bars" />
            </button>
          </div>

          <h1 class="menu-title">The WEAVE Processor</h1>
        </div>

        <div id="content" class="content">
          <main id="main" v-html="mainContent"></main>
        </div>
      </div>
    </div>

    <ModalManager
      ref="modalManager"
      @gotoModule="onGotoModule"
      @startPaging="onStartPaging"
    ></ModalManager>

    <PagerBar
      ref="pagerBar"
      :items="pagerItems"
      :current="currentModule"
      v-show="pagerItems.length > 0"
      @gotoModule="onGotoModule"
    ></PagerBar>
  </div>
</template>

<style src="./ttw-style.scss"></style>

<script setup lang="ts">
import { ref, watch, onMounted, onUnmounted } from "vue";
import { ModuleId, ModalKind } from "./base";
import { ModuleCache } from "./module-cache";
import { N_MODULES } from "./ttw/ttwModuleCount";
import ModalManager from "./ModalManager.vue";
import PagerBar from "./PagerBar.vue";

const modalManager = ref();
const pagerBar = ref();

// Current module display

const cache = new ModuleCache();
const desiredModule = ref(0);
const currentModule = ref(0);
const mainContent = ref("<div>Loading ...</div>");

watch(desiredModule, async (mid: ModuleId) => {
  mainContent.value = await cache.get(mid);
  currentModule.value = mid;
});

function showNext() {
  const cur = currentModule.value;
  const upd = cur < N_MODULES ? cur + 1 : N_MODULES;
  desiredModule.value = upd;
}

function showPrev() {
  const cur = currentModule.value;
  const upd = cur > 1 ? cur - 1 : 1;
  desiredModule.value = upd;
}

// Pager bar management

const pagerItems = ref<ModuleId[]>([]);

// Global keybindings

function handleSpacebar(event: KeyboardEvent) {
  if (event.shiftKey) {
    // shift-spacebar: scroll up
    if (window.scrollY <= 0) {
      showPrev();
      event.preventDefault();
    }
  } else {
    // Scrolling down is harder to compute. Some examples online use
    // `document.body` settings too, but those don't work here due to the way
    // our disappearing title bar works.
    const pageHeight = Math.max(
      document.documentElement.scrollHeight,
      document.documentElement.offsetHeight
    );

    // Most parameters here are ints but scrollY is a float, and if we don't
    // round the atBottom test can incorrectly fail.
    const atBottom =
      window.innerHeight + Math.round(window.scrollY) >= pageHeight;

    if (atBottom) {
      showNext();
      window.scroll({ top: 0 });
      event.preventDefault();
    }
  }
}

const keydownHandlers = {
  " ": handleSpacebar,

  ArrowLeft: (event: KeyboardEvent) => {
    event.preventDefault();
    showPrev();
  },

  ArrowRight: (event: KeyboardEvent) => {
    event.preventDefault();
    showNext();
  },

  ">": (event: KeyboardEvent) => {
    event.preventDefault();
    desiredModule.value = N_MODULES;
  },

  "<": (event: KeyboardEvent) => {
    event.preventDefault();
    desiredModule.value = 1;
  },

  //c: (event: KeyboardEvent) => {
  //  event.preventDefault();
  //  modalManager.value?.toggleModal(ModalKind.Contents);
  //},

  f: (event: KeyboardEvent) => {
    event.preventDefault();

    if (pagerItems.value.length > 0) {
      desiredModule.value = pagerItems.value[0];
    }
  },

  g: (event: KeyboardEvent) => {
    event.preventDefault();
    modalManager.value?.toggleGoto();
  },

  i: (event: KeyboardEvent) => {
    event.preventDefault();
    modalManager.value?.toggleIndex();
  },

  l: (event: KeyboardEvent) => {
    event.preventDefault();

    if (pagerItems.value.length > 0) {
      desiredModule.value = pagerItems.value[pagerItems.value.length - 1];
    }
  },

  n: (event: KeyboardEvent) => {
    event.preventDefault();
    pagerBar.value?.onClickNext();
  },

  p: (event: KeyboardEvent) => {
    event.preventDefault();
    pagerBar.value?.onClickPrev();
  },
};

function onKeydown(event: KeyboardEvent) {
  const handler = keydownHandlers[event.key];
  if (handler !== undefined) {
    handler(event);
  }
}

function mountKeybindings() {
  window.addEventListener("keydown", onKeydown);
}

function unmountKeybindings() {
  window.removeEventListener("keydown", onKeydown);
}

// Subcomponent event handlers

function onGotoModule(mid: ModuleId) {
  desiredModule.value = mid;
}

function onStartPaging(mids: ModuleId[]) {
  if (mids.length > 0) {
    pagerItems.value = mids;
    desiredModule.value = mids[0];
  }
}

// Quasi-hack -- globals invoked by the TeX-generated code.

globalThis.ttWeaveModRefOnClick = function ttWeaveModRefOnClick(mid: number) {
  modalManager.value?.showModuleInfo(mid);
};

// The hooks

onMounted(() => {
  mountKeybindings();

  desiredModule.value = 1;
});

onUnmounted(() => {
  unmountKeybindings();
});
</script>