<template>
  <div class="content-aligned">
    <ul class="actions">
      <li>
        <button class="icon-button" type="button" @click="onBack">
          <FontAwesomeIcon icon="fa-solid fa-bars" /> Back to text
        </button>
      </li>
      <li>
        <button
          class="icon-button"
          type="button"
          @click="emit('showCurrentModInfo')"
        >
          <FontAwesomeIcon icon="fa-solid fa-bars" /> Current module info
        </button>
      </li>
    </ul>

    <ul class="actions">
      <li>
        <button class="icon-button" type="button" @click="onContents">
          <FontAwesomeIcon icon="fa-solid fa-bars" /> Table of contents
        </button>
      </li>
      <li>
        <button class="icon-button" type="button" @click="onIndex">
          <FontAwesomeIcon icon="fa-solid fa-bars" /> Index of symbols
        </button>
      </li>
    </ul>

    <ul class="actions">
      <li>
        <button class="icon-button" type="button" @click="onKeybindings">
          <FontAwesomeIcon icon="fa-solid fa-bars" /> Keyboard shortcuts
        </button>
      </li>
    </ul>
  </div>
</template>

<style lang="scss" scoped>
.actions {
  width: 100%;
  padding: 0;
  margin-top: 2rem;

  li {
    width: 100%;
    margin: 2px 0;
    border: 1px solid var(--searchbar-border-color);
    border-radius: 3px;
    padding: 2px 6px;

    display: block;

    button {
      width: 100%;
      height: 100%;
      text-align: left;

      &:hover {
        cursor: pointer;
      }
    }
  }
}
</style>

<script setup lang="ts">
import { ModalKind } from "./base";

const emit = defineEmits<{
  (e: "doModal", kind: ModalKind): void;
  (e: "showCurrentModInfo"): void;
}>();

function onBack() {
  emit("doModal", ModalKind.None);
}

function onContents() {
  emit("doModal", ModalKind.Contents);
}

function onKeybindings() {
  emit("doModal", ModalKind.Keybindings);
}

function onIndex() {
  emit("doModal", ModalKind.Index);
}
</script>
