<template>
  <div class="content-aligned">
    <h1>{{ heading }}</h1>

    <p>{{ defSummary }}</p>

    <ul class="reflist">
      <li v-for="mid in defItems" :key="mid">
        <a @click="onRefClick(mid)">{{ mid }}</a>
      </li>
      <li v-if="defItems.length > 1">
        <a @click="onPageDefs">page</a>
      </li>
    </ul>

    <p>{{ refSummary }}</p>

    <ul class="reflist">
      <li v-for="mid in refItems" :key="mid">
        <a @click="onRefClick(mid)">{{ mid }}</a>
      </li>
      <li v-if="defItems.length > 1">
        <a @click="onPageRefs">page</a>
      </li>
    </ul>
  </div>
</template>

<style lang="scss" scoped>
.reflist {
  display: flex;
  flex-flow: row wrap;

  li {
    border: 1px solid #000;
    width: 3rem;
    height: 2rem;
    margin: 0.2rem;
    display: block;
    line-height: 2rem;
    text-align: center;

    a {
      display: block;
      width: 100%;
      height: 100%;

      &:hover {
        background: #eee;
        cursor: pointer;
      }
    }
  }
}
</style>

<script setup lang="ts">
import { ref, computed } from "vue";
import { ModuleId } from "./base";
import { getNamedModuleIndexEntry } from "./named-module-index";

const props = defineProps<{
  module?: ModuleId;
}>();

const emit = defineEmits<{
  (e: "goto", mid: ModuleId): void;
  (e: "startPaging", mids: ModuleId[]): void;
}>();

const ninfo = computed(() => {
  return getNamedModuleIndexEntry(props.module || 0);
});

const heading = computed(() => {
  if (props.module === 0) {
    return "Preamble";
  } else if (ninfo.value === undefined) {
    return `Module ${props.module}`;
  } else {
    return `§${props.module}: ${ninfo.value.n}`;
  }
});

const defItems = computed(() => {
  return ninfo.value?.d || [];
});

const defSummary = computed(() => {
  if (defItems.value.length == 1) {
    return "This named module is defined in one place:";
  } else {
    return `This named module is defined in ${defItems.value.length} places:`;
  }
});

const refItems = computed(() => {
  return ninfo.value?.r || [];
});

const refSummary = computed(() => {
  if (refItems.value.length == 1) {
    return "This named module is referenced in one place:";
  } else {
    return `This named module is referenced in ${refItems.value.length} places:`;
  }
});

function onRefClick(mid: ModuleId) {
  emit("goto", mid);
}

function onPageDefs() {
  emit("startPaging", defItems.value);
}

function onPageRefs() {
  emit("startPaging", refItems.value);
}
</script>
