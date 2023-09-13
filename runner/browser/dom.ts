import {
  Viewport,
  DomElement,
  DomError,
  SetViewportOptions,
  SetViewportOfOptions,
} from "browser";

export function focus(id: string): void | DomError {
  return withDomNode(id, (el) => el.focus());
}

export function blur(id: string): void | DomError {
  return withDomNode(id, (el) => el.blur());
}

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

export function setViewport(options: SetViewportOptions): void {
  window.scroll(options.y, options.y);
}

export function setViewportOf(options: SetViewportOfOptions): void | DomError {
  return withDomNode(options.id, (el) => {
    el.scrollLeft = options.x;
    el.scrollTop = options.y;
  });
}

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
