<template>
  <div class="pager-bar sticky bordered">
    <ul>
      <li v-for="mid in props.items" :key="mid">
        {{ mid }}
      </li>
    </ul>
  </div>
</template>

<style lang="scss" scoped>
.pager-bar {
  z-index: 101;
  position: fixed;
  bottom: 0;
  right: 0;

  /* bad hack */
  width: calc(100% - var(--sidebar-width));

  display: flex;
  flex-wrap: wrap;
  background-color: var(--bg);
  border-top-color: var(--table-border-color);
  border-top-width: 1px;
  border-top-style: solid;

  /* temporary hack */
  p {
    width: 100%;
    text-align: center;
  }
}
</style>

<script setup lang="ts">
import { ref } from "vue";
import { ModuleId } from "./base";

const props = defineProps<{
  items?: ModuleId[];
}>();

const emit = defineEmits<{
  (e: "gotoModule", mid: ModuleId): void;
}>();
</script>
