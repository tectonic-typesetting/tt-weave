// In the history, there is code where this worked with HTMLElements instead of
// strings. But to insert the content into the document in App.vue, the only
// Vue-esque way I can find that works is to use the special v-html property,
// which only seems to accept string inputs. Oh well.

import { ModuleId } from "./base";

export class ModuleCache {
  source: { [key: ModuleId]: string };

  constructor() {
    this.source = {};
  }

  async get(mid: ModuleId): Promise<string> {
    let cur = this.source[mid];

    if (cur !== undefined) {
      return cur;
    }

    const resp = await fetch(`ttw/module${mid}.html`);
    const text = await resp.text();
    this.source[mid] = text;
    return text;
  }

  getSync(mid: ModuleId): string | undefined {
    return this.source[mid];
  }
}
