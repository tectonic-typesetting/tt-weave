<template>
  <div>
    <div class="content-aligned">
      <h1>Index</h1>
    </div>

    <div class="modal-content-scrollbox">
      <div class="content-aligned">
        <ul class="index">
          <li v-for="sym in getSymbols()" :key="sym">{{ sym }}</li>
        </ul>
      </div>
    </div>
  </div>
</template>

<style lang="scss" scoped>
.modal-content-scrollbox {
  height: calc(100vh - 6rem);
  overflow-y: auto;
}

.index {
  margin-top: 0;
}
</style>

<script setup lang="ts">
import { ref } from "vue";
import { getSymbolIndexEntry, getSymbols } from "./symbol-index";
</script>