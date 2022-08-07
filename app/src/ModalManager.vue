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
        <GotoModal ref="goto"></GotoModal>
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
import { ModalKind } from "./base";
import ContentsModal from "./ContentsModal.vue";
import GotoModal from "./GotoModal.vue";

const active = ref(ModalKind.None);
const goto = ref();

function toggleGoto() {
  if (active.value == ModalKind.Goto) {
    active.value = ModalKind.None;
  } else {
    goto.value?.prepForShow();
    active.value = ModalKind.Goto;
  }
}

defineExpose({
  toggleGoto,
});
</script>
