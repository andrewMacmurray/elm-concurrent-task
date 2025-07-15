import {
  Viewport,
  DomElement,
  DomError,
  SetViewportOptions,
  SetViewportOfOptions,
} from "./index.js";

// Relevant Elm Kernel code: https://github.com/elm/browser/blob/master/src/Elm/Kernel/Browser.js#L322-L328
// Note: `focus` is called using `Elm.Kernel.Browser.call "focus"`
export function focus(id: string): void | DomError {
  return withDomNode(id, (el) => el.focus());
}

// Relevant Elm Kernel code: https://github.com/elm/browser/blob/master/src/Elm/Kernel/Browser.js#L322-L328
// Note: `blur` is called using `Elm.Kernel.Browser.call "blur"`
export function blur(id: string): void | DomError {
  return withDomNode(id, (el) => el.blur());
}

// Equivalent Elm Kernel code: https://github.com/elm/browser/blob/master/src/Elm/Kernel/Browser.js#L335-L346
export function getViewport(): Viewport {
  return {
    scene: getBrowserScene(),
    viewport: {
      x: window.scrollX,
      y: window.scrollY,
      width: document.documentElement.clientWidth,
      height: document.documentElement.clientHeight,
    },
  };
}

// Equivalent Elm Kernel code: https://github.com/elm/browser/blob/master/src/Elm/Kernel/Browser.js#L372-L389
export function getViewportOf(id: string): Viewport | DomError {
  return withDomNode(id, (el) => ({
    scene: {
      width: el.scrollWidth,
      height: el.scrollHeight,
    },
    viewport: {
      x: el.scrollLeft,
      y: el.scrollTop,
      width: el.clientWidth,
      height: el.clientHeight,
    },
  }));
}

// Equivalent Elm Kernel code: https://github.com/elm/browser/blob/master/src/Elm/Kernel/Browser.js#L358-L365
export function setViewport(options: SetViewportOptions): void {
  window.scroll(options.y, options.y);
}

// Equivalent Elm Kernel code: https://github.com/elm/browser/blob/master/src/Elm/Kernel/Browser.js#L392-L400
export function setViewportOf(options: SetViewportOfOptions): void | DomError {
  return withDomNode(options.id, (el) => {
    el.scrollLeft = options.x;
    el.scrollTop = options.y;
  });
}

// Equivalent Elm Kernel code: https://github.com/elm/browser/blob/master/src/Elm/Kernel/Browser.js#L407-L430
export function getElement(id: string): DomElement | DomError {
  return withDomNode(id, (el) => {
    const rect = el.getBoundingClientRect();
    const x = window.scrollX;
    const y = window.scrollY;
    return {
      scene: getBrowserScene(),
      viewport: {
        x: x,
        y: y,
        width: document.documentElement.clientWidth,
        height: document.documentElement.clientHeight,
      },
      element: {
        x: x + rect.left,
        y: y + rect.top,
        width: rect.width,
        height: rect.height,
      },
    };
  });
}

// Helpers

// Equivalent Elm Kernel code: https://github.com/elm/browser/blob/master/src/Elm/Kernel/Browser.js#L293-L305
function withDomNode<a>(
  id: string,
  callback: (el: HTMLElement) => a
): a | DomError {
  const el = document.getElementById(id);
  if (el) {
    return callback(el);
  }
  return { error: null };
}

// Equivalent Elm Kernel code: https://github.com/elm/browser/blob/master/src/Elm/Kernel/Browser.js#L348-L356
function getBrowserScene(): { width: number; height: number } {
  const body = document.body;
  const elem = document.documentElement;
  return {
    width: Math.max(
      body.scrollWidth,
      body.offsetWidth,
      elem.scrollWidth,
      elem.offsetWidth,
      elem.clientWidth
    ),
    height: Math.max(
      body.scrollHeight,
      body.offsetHeight,
      elem.scrollHeight,
      elem.offsetHeight,
      elem.clientHeight
    ),
  };
}
