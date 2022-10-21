<template>
  <div>
    <div class="content-aligned">
      <h1>Index</h1>
    </div>

    <div class="modal-content-scrollbox">
      <nav class="content-aligned">
        <IndexEntry
          v-for="sym in symbols"
          :key="sym"
          :name="sym"
          @gotoModule="onGotoModule"
          @startPaging="onStartPaging"
        />
      </nav>
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
import { ModuleId } from "./base";
import { getSymbols } from "./symbol-index";
import IndexEntry from "./IndexEntry.vue";

const symbols = ref<string[]>([]);

const emit = defineEmits<{
  (e: "goto", mid: ModuleId): void;
  (e: "startPaging", mids: ModuleId[]): void;
}>();

function onGotoModule(mid: ModuleId) {
  emit("goto", mid);
}

function onStartPaging(mids: ModuleId[]) {
  emit("startPaging", mids);
}

function prepForShow() {
  // This is kind of lame, but we have no reactive way to know if/when to
  // re-check whether the index has loaded!

  if (!symbols.value.length) {
    symbols.value = getSymbols();
  }
}

defineExpose({
  prepForShow,
});
</script>