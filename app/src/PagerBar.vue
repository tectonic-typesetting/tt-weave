<template>
  <div class="pager-bar sticky bordered">
    <ul class="items">
      <li v-if="!canFitAll" :class="{ disabled: prevModuleId < 0 }">
        <a :class="{ disabled: prevModuleId < 0 }" @click="onClickPrev">&lt;</a>
      </li>
      <li
        v-for="mid in subset"
        :key="mid"
        :class="{ selected: current == mid }"
      >
        <a @click="onClickModule(mid)">{{ mid }}</a>
      </li>
      <li v-if="!canFitAll" :class="{ disabled: nextModuleId < 0 }">
        <a :class="{ disabled: nextModuleId < 0 }" @click="onClickNext">&gt;</a>
      </li>
      <ResizeObserver @notify="onResize" />
    </ul>
  </div>
</template>

<style lang="scss" scoped>
@import "npm:vue-resize/dist/vue-resize.css";

.pager-bar {
  z-index: 101;

  position: fixed;
  bottom: 0;
  right: 0;
  left: 0;

  display: flex;
  flex-wrap: wrap;
  background-color: var(--bg);
  border-top-color: var(--table-border-color);
  border-top-width: 1px;
  border-top-style: solid;
}

.items {
  display: flex;
  flex-flow: row wrap;
  justify-content: center;
  max-height: 48px;
  overflow-y: hide;
  position: relative;
  width: 100%;
  padding: 0;

  li {
    // If changing these, update ITEM_WIDTH_PX below!
    border: 1px solid #000;
    width: 54px;
    height: 36px;
    margin: 3px;
    display: block;
    line-height: 36px;
    text-align: center;

    &.selected {
      background: #ccf;
    }

    &.disabled {
      opacity: 0.3;
    }

    a {
      display: block;
      width: 100%;
      height: 100%;

      &:hover {
        background: #eee;
        cursor: pointer;
      }

      &.disabled:hover {
        cursor: default;
      }
    }
  }
}
</style>

<script setup lang="ts">
import { ref, computed, watch } from "vue";
import { ModuleId } from "./base";

const props = defineProps<{
  items?: ModuleId[];
  current: ModuleId;
}>();

const emit = defineEmits<{
  (e: "gotoModule", mid: ModuleId): void;
}>();

const barWidth = ref(300);

function onResize({ width }: { width: number }) {
  console.log("resize", width);
  barWidth.value = width;
}

const ITEM_WIDTH_PX = 62; // 54 + 2*1px border + 2*3px margin

// Can we git all of the `items` in the current view? If so, no arrow buttons
// needed.
const canFitAll = computed(() => {
  return props.items.length * ITEM_WIDTH_PX < barWidth.value;
});

// The index into `items` that defines the start of the `subset` array that is
// currently being displayed.
const currentSubsetStartIndex = ref(0);

// The subset of `items` currently being displayed in the pager. If `canFitAll`,
// then it's everything. If not, it's the subset starting at
// `currentSubsetStartIndex` that fits, taking into account the fact that we'll
// show two additional "items" for left and right arrows to scroll through the
// list.
const subset = computed(() => {
  const items = props.items || [];

  if (canFitAll.value) {
    return items;
  }

  const n = Math.floor(barWidth.value / ITEM_WIDTH_PX);
  const i0 = currentSubsetStartIndex.value;
  const d = Math.max(1, n - 2);
  const i1 = Math.min(i0 + d, items.length);
  return items.slice(i0, i1);
});

// The index into `items` corresponding to `props.current`, the item that is
// currently being viewed, or -1 if the currently viewed item is not in `items`.
const currentlyViewedIndex = computed(() => {
  for (const [idx, mid] of (props.items || []).entries()) {
    if (props.current == mid) {
      return idx;
    }
  }

  return -1;
});

// If the index updates to something not visible within the current
// subset, adjust `currentSubsetStartIndex` to make it visible.
watch(currentlyViewedIndex, (idx: number) => {
  // If the new index isn't within the set of modules being paged, do nothing.
  if (idx < 0) {
    return;
  }

  // If the new index is within the `subset` currently being viewed, no need to
  // shift: do nothing.
  const ofs = idx - currentSubsetStartIndex.value;
  if (ofs >= 0 && ofs < subset.value.length) {
    return;
  }

  // If the new index is smaller, move in that direction.
  if (ofs < 0) {
    currentSubsetStartIndex.value = idx;
    return;
  }

  // Otherwise, it must be too big. Adjust accordingly.
  currentSubsetStartIndex.value = idx + 1 - subset.value.length;
});

// The module ID that we should seek to if the user presses the "next" button,
// or -1 if no such seek should happen. This is the first module ID in `items`
// that is bigger than `current`.
const nextModuleId = computed(() => {
  for (const mid of props.items) {
    if (mid > props.current) {
      return mid;
    }
  }

  return -1;
});

// Like `nextModuleId`, but for the "prev" button.
const prevModuleId = computed(() => {
  var prev = -1;

  for (const mid of props.items) {
    if (mid >= props.current) {
      return prev;
    }

    prev = mid;
  }

  return prev;
});

function onClickModule(mid: ModuleId) {
  emit("gotoModule", mid);
}

function onClickPrev() {
  if (prevModuleId.value >= 0) {
    emit("gotoModule", prevModuleId.value);
  }
}

function onClickNext() {
  if (nextModuleId.value >= 0) {
    emit("gotoModule", nextModuleId.value);
  }
}

defineExpose({
  onClickPrev,
  onClickNext,
});
</script>
