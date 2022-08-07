<template>
  <div>
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
  display: none;
  background-color: #fff;

  &.modal-container-visible {
    display: block;
  }

  .content-aligned {
    margin-left: auto;
    margin-right: auto;
    max-width: var(--content-max-width);
  }
}
</style>

<script setup lang="ts">
import { ref } from "vue";
import { ModalKind } from "./base";

const desiredModal = ref(ModalKind.None);

function toggleModal(kind: ModalKind) {
  console.log("toggle: " + kind);
}

defineExpose({
  toggleModal,
});
</script>
