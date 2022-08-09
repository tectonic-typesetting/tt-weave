<template>
  <div>
    <div
      :class="{
        'modal-overlay': true,
        'modal-overlay-visible': active != ModalKind.None,
      }"
    ></div>

    <div class="modal-wrapper">
      <div
        v-show="active == ModalKind.Contents"
        class="modal-container page-wrapper"
      >
        <ContentsModal></ContentsModal>
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
        <IndexModal ref="index"></IndexModal>
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
  background-color: rgba(255, 255, 255, 0.95);

  &.modal-overlay-visible {
    display: block;
  }
}

.modal-wrapper {
  position: fixed;
  top: 0;
  width: 100%;
  z-index: 201;
}

.modal-container {
  background-color: #fff;

  .content-aligned {
    margin-left: auto;
    margin-right: auto;
    max-width: var(--content-max-width);
  }
}
</style>

<script setup lang="ts">
import { ref, computed } from "vue";
import { ModalKind, ModuleId } from "./base";
import ContentsModal from "./ContentsModal.vue";
import GotoModal from "./GotoModal.vue";
import IndexModal from "./IndexModal.vue";
import ModuleInfoModal from "./ModuleInfoModal.vue";

const emit = defineEmits<{
  (e: "gotoModule", mid: ModuleId): void;
  (e: "startPaging", mids: ModuleId[]): void;
}>();

const active = ref(ModalKind.None);
const goto = ref();
const index = ref();
const modinfo = ref();
const modinfoCurModule = ref(0);

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
    active.value = ModalKind.Index;
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

defineExpose({
  showModuleInfo,
  toggleGoto,
  toggleIndex,
});
</script>
