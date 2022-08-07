import { createApp } from "vue";
import App from "./App.vue";

const app = createApp(App);
app.mount("#app");

globalThis.ttWeaveModRefOnClick = function ttWeaveModRefOnClick(mid: number) {
  console.log("modrefclick!");
}
