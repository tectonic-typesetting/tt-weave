import { createApp } from "vue";
import { ResizeObserver } from 'vue-resize'
import App from "./App.vue";

const app = createApp(App);
app.component("ResizeObserver", ResizeObserver);
app.mount("#app");
