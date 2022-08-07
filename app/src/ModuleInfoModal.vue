<template>
  <div class="content-aligned">
    <h1>Module Info</h1>
  </div>
</template>

<style lang="scss" scoped>
</style>

<script setup lang="ts">
import { ref } from "vue";
import { ModuleId } from "./base";

function prepForShow(mid: ModuleId) {}

defineExpose({
  prepForShow,
});
</script>
