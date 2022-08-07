import { ModuleId } from "./base";

export class ModuleCache {
  source: { [key: ModuleId]: HTMLElement };

  constructor() {
    this.source = {};
  }

  async get(mid: ModuleId): Promise<HTMLElement> {
    let cur = this.source[mid];

    if (cur !== undefined) {
      return cur;
    }

    const resp = await fetch(`ttw/module${mid}.html`);
    const text = await resp.text();
    const doc = new DOMParser().parseFromString(text, "text/html");
    this.source[mid] = doc.documentElement;
    return doc.documentElement;
  }

  getSync(mid: ModuleId): HTMLElement | undefined {
    return this.source[mid];
  }
}
