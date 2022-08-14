import { createApp } from "vue";
import { ResizeObserver } from "vue-resize";
import App from "./App.vue";

import { library } from "@fortawesome/fontawesome-svg-core";
import { FontAwesomeIcon } from "@fortawesome/vue-fontawesome";
import { faBars } from "@fortawesome/free-solid-svg-icons";

library.add(faBars)

const app = createApp(App);
app.component("ResizeObserver", ResizeObserver);
app.component("FontAwesomeIcon", FontAwesomeIcon);
app.mount("#app");
