<template>
  <div class="content-aligned">
    <h1>Go To Module</h1>

    <div>
      <form @submit.prevent="onSubmit">
        <!-- This is a numeric entry, but UX-wise I think it's better to code it as text -->
        <div>
          <label for="goto-modal-entry">Enter a module number:</label>
        </div>
        <div>
          <input
            ref="input"
            v-model="text"
            type="text"
            id="goto-modal-entry"
            name="goto-modal-entry"
          />
        </div>
        <div>
          <p class="goto-modal-error" v-show="errorText.length > 0">
            {{ errorText }}
          </p>
        </div>
        <div>
          <button>Go</button>
        </div>
      </form>
    </div>
  </div>
</template>

<style lang="scss" scoped>
input {
  font-size: larger;
  margin-top: 1rem;
}

button {
  margin-top: 1rem;
}

.goto-modal-error {
  display: block;
  margin: 1rem 0 0 0;
  color: #bd2020;
}
</style>

<script setup lang="ts">
import { ref, nextTick } from "vue";
import { ModuleId } from "./base";
import { N_MODULES } from "./ttw/ttwModuleCount";

const input = ref<HTMLInputElement | null>(null);
const text = ref("");
const errorText = ref("");

function prepForShow() {
  errorText.value = "";
  text.value = "";

  // Gross. Should find a better approach here.
  nextTick(() => {
    nextTick(() => {
      input.value?.focus();
    });
  });
}

const emit = defineEmits<{
  (e: "goto", mid: ModuleId): void;
}>();

function onSubmit() {
  const mid = parseInt(text.value, 10);

  if (mid >= 1 && mid <= N_MODULES) {
    input.value?.blur();
    text.value = "";
    errorText.value = "";
    emit("goto", mid);
  } else {
    errorText.value = `“${text.value}” doesn’t look like a valid module number.`;
    text.value = "";
  }
}

defineExpose({
  prepForShow,
});
</script>
