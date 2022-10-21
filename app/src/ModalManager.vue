<template>
  <div>
    <div
      :class="{
        'modal-overlay': true,
        'modal-overlay-visible': active != ModalKind.None,
      }"
    ></div>

    <div
      :class="{
        'modal-wrapper': true,
        'modal-wrapper-visible': active != ModalKind.None,
      }"
    >
      <div
        v-show="active == ModalKind.Contents"
        class="modal-container page-wrapper"
      >
        <ContentsModal></ContentsModal>
      </div>

      <div
        v-show="active == ModalKind.Dispatch"
        class="modal-container page-wrapper"
      >
        <DispatchModal
          @doModal="onDoModal"
          @showCurrentModInfo="emit('showCurrentModInfo')"
        ></DispatchModal>
      </div>

      <div
        v-show="active == ModalKind.Goto"
        class="modal-container page-wrapper"
      >
        <GotoModal ref="goto" @goto="onGoto"></GotoModal>
      </div>

      <div
        v-show="active == ModalKind.Index"
        class="modal-container page-wrapper"
      >
        <IndexModal
          ref="index"
          @goto="onGoto"
          @startPaging="onStartPaging"
        ></IndexModal>
      </div>

      <div
        v-show="active == ModalKind.Keybindings"
        class="modal-container page-wrapper"
      >
        <KeybindingsModal></KeybindingsModal>
      </div>

      <div
        v-show="active == ModalKind.ModuleInfo"
        class="modal-container page-wrapper"
      >
        <ModuleInfoModal
          ref="modinfo"
          :module="modinfoCurModule"
          @goto="onGoto"
          @startPaging="onStartPaging"
        ></ModuleInfoModal>
      </div>

      <button
        type="button"
        @click="clear"
        class="close-button"
        title="Close overlay"
        aria-label="Close overlay"
      >
        ×
      </button>
    </div>
  </div>
</template>

<style lang="scss">
// Derived from
// https://rapaccinim.medium.com/how-to-create-a-custom-resizable-modal-with-scrollable-and-fixed-content-21adb2adda28

.modal-overlay {
  display: none;
  position: fixed;
  top: 0;
  left: 0;
  z-index: 200;
  width: 100%;
  height: 100%;
  background-color: rgba(255, 255, 255, 0.9);
  backdrop-filter: blur(1px);

  &.modal-overlay-visible {
    display: block;
  }
}

.modal-wrapper {
  display: none;
  position: fixed;
  top: 0;
  left: 0;
  right: 0;
  height: 100%;
  padding: 0 var(--page-padding);

  z-index: 201;

  &.modal-wrapper-visible {
    display: block;
  }
}

.modal-container {
  background-color: #fff;

  // Standardize this for content scrollbox height computation.
  h1 {
    margin: 2rem 0;
    line-height: 2rem;
  }

  .content-aligned {
    margin-left: auto;
    margin-right: auto;
    max-width: var(--content-max-width);
  }
}

.close-button {
  position: fixed;
  top: 0.5rem;
  right: 0.5rem;
  font-size: 2em;
  width: 3rem;
  height: 3rem;
  border: none;
  border-radius: 5px;

  background-color: #fff;
  color: var(--icons);

  &:hover {
    cursor: pointer;
    color: var(--icons-hover);
  }
}
</style>

<script setup lang="ts">
import { ref, computed } from "vue";
import { ModalKind, ModuleId } from "./base";
import ContentsModal from "./ContentsModal.vue";
import DispatchModal from "./DispatchModal.vue";
import GotoModal from "./GotoModal.vue";
import IndexModal from "./IndexModal.vue";
import KeybindingsModal from "./KeybindingsModal.vue";
import ModuleInfoModal from "./ModuleInfoModal.vue";

const emit = defineEmits<{
  (e: "gotoModule", mid: ModuleId): void;
  (e: "showCurrentModInfo"): void;
  (e: "startPaging", mids: ModuleId[]): void;
}>();

const active = ref(ModalKind.None);
const goto = ref();
const index = ref();
const modinfo = ref();
const modinfoCurModule = ref(0);

function clear() {
  active.value = ModalKind.None;
}

function toggleContents() {
  if (active.value == ModalKind.Contents) {
    active.value = ModalKind.None;
  } else {
    active.value = ModalKind.Contents;
  }
}

function toggleDispatch() {
  if (active.value == ModalKind.Dispatch) {
    active.value = ModalKind.None;
  } else {
    active.value = ModalKind.Dispatch;
  }
}

function toggleGoto() {
  if (active.value == ModalKind.Goto) {
    active.value = ModalKind.None;
  } else {
    goto.value?.prepForShow();
    active.value = ModalKind.Goto;
  }
}

function toggleIndex() {
  if (active.value == ModalKind.Index) {
    active.value = ModalKind.None;
  } else {
    index.value?.prepForShow();
    active.value = ModalKind.Index;
  }
}

function toggleKeybindings() {
  if (active.value == ModalKind.Keybindings) {
    active.value = ModalKind.None;
  } else {
    active.value = ModalKind.Keybindings;
  }
}

function showModuleInfo(mid: ModuleId) {
  modinfoCurModule.value = mid;
  active.value = ModalKind.ModuleInfo;
}

function onGoto(mid: ModuleId) {
  emit("gotoModule", mid);
  active.value = ModalKind.None;
}

function onStartPaging(mids: ModuleId[]) {
  emit("startPaging", mids);
  active.value = ModalKind.None;
}

function onDoModal(kind: ModalKind) {
  active.value = kind;
}

defineExpose({
  clear,
  showModuleInfo,
  toggleContents,
  toggleDispatch,
  toggleGoto,
  toggleIndex,
  toggleKeybindings,
});
</script>
