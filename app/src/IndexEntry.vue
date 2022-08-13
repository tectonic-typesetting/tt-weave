<template>
  <div class="index-entry">
    <a class="index-entry-name">{{ props.name }}</a>
    <ul>
      <li class="entry-def" v-for="mid in defs" :key="mid">
        <a @click="onClickModule(mid)">{{ mid }}</a>
      </li>
      <li class="entry-ref" v-for="mid in refs" :key="mid">
        <a @click="onClickModule(mid)">{{ mid }}</a>
      </li>
      <li v-if="refs.length + defs.length > 1">
        <a @click="onPageIt">page</a>
      </li>
    </ul>
  </div>
</template>

<style lang="scss" scoped>
.index-entry {
  width: 100%;
  margin: 0.4rem 0;
  padding: 3px;
  border: 1px solid rgba(0, 0, 0, 0);
  display: flex;

  // Remember, no hovering on mobile!
  &:hover {
    border: 1px solid var(--table-border-color);
    background-color: var(--table-alternate-bg);
  }

  .index-entry-name {
    line-height: 1em;
    flex: 1;
  }

  ul {
    flex: 1;
    margin: 0;
    padding: 0;

    display: flex;
    flex-flow: row wrap;

    li {
      border: 1px solid #000;
      width: 54px;
      height: 36px;
      margin: 3px;
      display: block;
      line-height: 36px;
      text-align: center;

      &.entry-def {
        text-decoration: underline;
      }

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
}
</style>

<script setup lang="ts">
import { computed } from "vue";
import { ModuleId } from "./base";
import { getSymbolIndexEntry } from "./symbol-index";

const props = defineProps<{
  name: string;
}>();

const emit = defineEmits<{
  (e: "gotoModule", mid: ModuleId): void;
  (e: "startPaging", mids: ModuleId[]): void;
}>();

const refs = computed(() => {
  return getSymbolIndexEntry(props.name).r;
});

const defs = computed(() => {
  return getSymbolIndexEntry(props.name).d;
});

function onClickModule(mid: ModuleId) {
  emit("gotoModule", mid);
}

function onPageIt() {
  const mids = [...refs.value, ...defs.value];
  mids.sort((n1, n2) => n1 - n2);
  emit("startPaging", mids);
}
</script>