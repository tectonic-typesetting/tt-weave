// Stub implementation

<template>
  <div class="content-aligned">
    <h1>Keyboard Shortcuts</h1>

    <ul class="keylist">
      <li>
        <span class="d"><span>Go to next module</span></span
        ><span class="k">→</span>
      </li>
      <li>
        <span class="d"><span>Go to previous module</span></span
        ><span class="k">←</span>
      </li>
      <li>
        <span class="d"><span>Go to first module</span></span
        ><span class="k">&lt;</span>
      </li>
      <li>
        <span class="d"><span>Go to last module</span></span
        ><span class="k">&gt;</span>
      </li>
    </ul>

    <ul class="keylist">
      <li>
        <span class="d"><span>Go to next module in sequence</span></span
        ><span class="k">n</span>
      </li>
      <li>
        <span class="d"><span>Go to previous module in sequence</span></span
        ><span class="k">p</span>
      </li>
      <li>
        <span class="d"><span>Go to first module in sequence</span></span
        ><span class="k">f</span>
      </li>
      <li>
        <span class="d"><span>Go to last module in sequence</span></span
        ><span class="k">l</span>
      </li>
    </ul>

    <ul class="keylist">
      <li>
        <span class="d"><span>Go to module</span></span
        ><span class="k">g</span>
      </li>
      <li>
        <span class="d"><span>Show/hide contents</span></span
        ><span class="k">c</span>
      </li>
      <li>
        <span class="d"><span>Show/hide index</span></span
        ><span class="k">i</span>
      </li>
      <li>
        <span class="d"><span>Show/hide this help</span></span
        ><span class="k">?</span>
      </li>
    </ul>
  </div>
</template>

<style lang="scss" scoped>
.keylist {
  width: 100%;
  padding: 0;
  margin-top: 2rem;

  li {
    width: 100%;
    margin: 2px 0;
    border: 1px solid var(--searchbar-border-color);
    border-radius: 3px;
    padding: 2px;
    box-sizing: border-box;

    display: flex;
    align-items: center;

    line-height: 18px;

    .d {
      flex: 1;
      height: 18px;

      span {
        display: inline-block;
        line-height: normal;
        vertical-align: middle;
      }
    }

    .k {
      flex: 0 30px;
      height: 18px;
      font-family: "tduxMono";
      font-size: 0.875em;

      span {
        display: inline-block;
        line-height: normal;
        vertical-align: middle;
      }
    }
  }
}
</style>