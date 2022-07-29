// Yay TypeScript!

export { }

// Stuff set up on init

const N_MODULES = 265;

type ModuleId = number;

interface NamedModuleIndexEntry {
  n: string;
  d: ModuleId[];
  r: ModuleId[];
}

declare var ttWeaveNamedModuleIndex: { [id: ModuleId]: NamedModuleIndexEntry } | undefined;

// Module cache

class ModuleCache {
  source: { [key: ModuleId]: HTMLElement };

  constructor() {
    this.source = {};
  }

  async get(mid: ModuleId): Promise<HTMLElement> {
    let cur = this.source[mid];

    if (cur !== undefined) {
      return cur;
    }

    const resp = await fetch(`module${mid}.html`);
    const text = await resp.text();
    const doc = new DOMParser().parseFromString(text, "text/html");
    this.source[mid] = doc.documentElement;
    return doc.documentElement;
  }
}

// Handling modals

class Modal {
  mgr: ModalManager;
  el: HTMLElement;
  visible: boolean;

  constructor(mgr: ModalManager, elid: string) {
    this.mgr = mgr;
    this.el = document.getElementById(elid)!;
    this.visible = false;
  }

  open() {
    if (this.visible) {
      return;
    }

    this.mgr.closeAll();
    this.el.classList.add("modal-container-visible");
    this.visible = true;
    document.body.style.overflow = "hidden";
    this.mgr.overlay.classList.add("modal-overlay-visible");
  }

  close() {
    if (!this.visible) {
      return;
    }

    this.el.classList.remove("modal-container-visible");
    this.visible = false;
  }

  toggle() {
    if (this.visible) {
      this.mgr.closeAll();
    } else {
      this.open();
    }
  }

}

class ModalManager {
  app: App;
  overlay: HTMLElement;

  // type-erased list for iterating over all
  byName: { [key: string]: Modal };

  // type-ful list for modal-specific capabilities:
  goto: GotoModal;
  modinfo: ModuleInfoModal;

  constructor(app: App) {
    this.app = app;
    this.overlay = document.getElementById("modal-overlay")!;
    this.byName = {};
    this.goto = this.register(GotoModal, "goto");
    this.modinfo = this.register(ModuleInfoModal, "modinfo");
  }

  register<T extends Modal>(cls: { new(m: ModalManager, e: string): T }, key: string): T {
    const modal = new cls(this, key + "-modal");
    this.byName[key] = modal;
    return modal;
  }

  closeAll() {
    for (const [_key, modal] of Object.entries(this.byName)) {
      modal.close();
    }

    document.body.style.overflow = "visible";
    this.overlay.classList.remove("modal-overlay-visible");
  }

  openModInfo(mid: ModuleId) {
    this.modinfo.prepOpen(mid);
    this.modinfo.open();
  }
}

class GotoModal extends Modal {
  form: HTMLFormElement;
  entry: HTMLInputElement;
  error: HTMLElement;

  constructor(mgr: ModalManager, elid: string) {
    super(mgr, elid);

    this.form = document.getElementById("goto-modal-form") as HTMLFormElement;
    this.entry = document.getElementById("goto-modal-entry") as HTMLInputElement;
    this.error = document.getElementById("goto-modal-error")!;

    this.form.addEventListener("submit", (event) => {
      event.preventDefault();

      const mid = parseInt(this.entry.value, 10);

      if (mid >= 1 && mid <= N_MODULES) {
        this.entry.blur();
        this.entry.value = "";
        this.mgr.app.showSingleModule(mid);
        this.mgr.closeAll();
      } else {
        this.error.innerText = `“${this.entry.value}” doesn’t look like a valid module number.`;
        this.error.classList.add("goto-modal-error-visible");
        this.entry.value = "";
      }
    });
  }

  open() {
    super.open();

    this.entry.value = "";
    this.entry.focus();
    this.error.classList.remove("goto-modal-error-visible");
    this.error.innerText = "";
  }
}

class ModuleInfoModal extends Modal {
  title: HTMLElement;
  defDesc: HTMLElement;
  defList: HTMLElement;
  refDesc: HTMLElement;
  refList: HTMLElement;

  constructor(mgr: ModalManager, elid: string) {
    super(mgr, elid);

    this.title = document.getElementById("modinfo-modal-title")!;
    this.defDesc = document.getElementById("modinfo-modal-def-desc")!;
    this.defList = document.getElementById("modinfo-modal-def-list")!;
    this.refDesc = document.getElementById("modinfo-modal-ref-desc")!;
    this.refList = document.getElementById("modinfo-modal-ref-list")!;
  }

  prepOpen(mid: ModuleId) {
    var rec = undefined;

    if (ttWeaveNamedModuleIndex !== undefined) {
      rec = ttWeaveNamedModuleIndex[mid];
    }

    if (rec == undefined) {
      // It's an anonymous module, or we haven't yet loaded the index. The only
      // thing we can really do here is go to it.
      this.mgr.app.showSingleModule(mid);
      return;
    }

    // OK, we have something we can work with!

    this.title.innerText = `§${mid}: ${rec.n}`;

    if (rec.d.length == 1) {
      this.defDesc.innerText = "This named module is defined in one place:";
    } else {
      this.defDesc.innerText = `This named module is defined in ${rec.d.length} places:`;
    }

    this.defList.innerHTML = "";

    for (const rmid of rec.d) {
      const a = document.createElement("a");
      a.className = "modref";
      a.innerText = "" + rmid;
      a.addEventListener("click", () => {
        this.mgr.app.showSingleModule(rmid);
        this.mgr.closeAll();
      });

      const li = document.createElement("li");
      li.appendChild(a);

      this.defList.appendChild(li);
    }

    if (rec.r.length == 1) {
      this.refDesc.innerText = "It is referenced in one place:";
    } else {
      this.refDesc.innerText = `It is referenced in ${rec.d.length} places:`;
    }

    this.refList.innerHTML = "";

    for (const rmid of rec.r) {
      const a = document.createElement("a");
      a.className = "modref";
      a.innerText = "" + rmid;
      a.addEventListener("click", () => {
        this.mgr.app.showSingleModule(rmid);
        this.mgr.closeAll();
      });

      const li = document.createElement("li");
      li.appendChild(a);

      this.refList.appendChild(li);
    }
  }
}

// The main application

class App {
  cache: ModuleCache;
  modals: ModalManager;
  main: HTMLElement;
  curModule: ModuleId;

  constructor() {
    this.cache = new ModuleCache();
    this.modals = new ModalManager(this);
    this.main = document.getElementById("main")!;
    this.curModule = 1;
  }

  async getModule(mid: ModuleId): Promise<HTMLElement> {
    const content = await this.cache.get(mid);

    const wrapper = document.createElement("div");
    wrapper.className = "ttw-module";
    wrapper.appendChild(content);
    return wrapper;
  }

  async showSingleModule(mid: ModuleId) {
    const div = await this.getModule(mid);
    this.main.innerHTML = "";
    this.main.appendChild(div);
    this.curModule = mid;
  }

  async showNextModule() {
    const mid = (this.curModule < N_MODULES) ? this.curModule + 1 : N_MODULES;
    return this.showSingleModule(mid);
  }

  async showPreviousModule() {
    const mid = (this.curModule > 1) ? this.curModule - 1 : 1;
    return this.showSingleModule(mid);
  }

  async handleSpacebar(event: KeyboardEvent) {
    if (event.shiftKey) {
      // shift-spacebar: scroll up
      if (window.scrollY <= 0) {
        this.showPreviousModule();
        event.preventDefault();
      }
    } else {
      // Scrolling down is harder to compute. Some examples online use
      // `document.body` settings too, but those don't work here due to the way
      // our disappearing title bar works.
      const pageHeight = Math.max(
        document.documentElement.scrollHeight,
        document.documentElement.offsetHeight
      );

      const atBottom = (window.innerHeight + window.scrollY) >= pageHeight;

      if (atBottom) {
        this.showNextModule();
        event.preventDefault();
      }
    }
  }

  async initialize() {
    this.showSingleModule(1);

    // Keybindings

    const b = document.body;

    b.addEventListener("keydown", async (event) => {
      if (event.key == "ArrowRight") {
        event.preventDefault();
        await this.showNextModule();
      } else if (event.key == "ArrowLeft") {
        event.preventDefault();
        await this.showPreviousModule();
      } else if (event.key == "Home") {
        event.preventDefault();
        await this.showSingleModule(1);
      } else if (event.key == "End") {
        event.preventDefault();
        await this.showSingleModule(N_MODULES);
      } else if (event.key == "g") {
        event.preventDefault();
        this.modals.goto.toggle();
      } else if (event.key == " ") {
        this.handleSpacebar(event);
      }
    });
  }
}

const app = new App();
app.initialize();

globalThis.ttWeaveModRefOnClick = function ttWeaveModRefOnClick(mid: ModuleId) {
  app.modals.openModInfo(mid);
}