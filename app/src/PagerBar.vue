<template>
  <div class="pager-bar sticky bordered">
    <ul class="items">
      <li v-for="mid in props.items" :key="mid">
        <a @click="onClickModule(mid)">{{ mid }}</a>
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

.items {
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
import { ref } from "vue";
import { ModuleId } from "./base";

const props = defineProps<{
  items?: ModuleId[];
}>();

const emit = defineEmits<{
  (e: "gotoModule", mid: ModuleId): void;
}>();

function onClickModule(mid: ModuleId) {
  emit("gotoModule", mid);
}
</script>
