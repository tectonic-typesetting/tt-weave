<template>
  <div class="content-aligned">
    <h1>Index</h1>

    <ul>
      <li v-for="sym in getSymbols()" :key="sym">{{ sym }}</li>
    </ul>
  </div>
</template>

<style lang="scss" scoped>
</style>

<script setup lang="ts">
import { ref } from "vue";
import { getSymbolIndexEntry, getSymbols } from "./symbol-index";
</script>