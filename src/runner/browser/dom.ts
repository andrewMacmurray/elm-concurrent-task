export type Error = { error: null };

export interface Viewport {
  scene: {
    width: number;
    height: number;
  };
  viewport: {
    x: number;
    y: number;
    width: number;
    height: number;
  };
}

export interface DomElement {
  scene: {
    width: number;
    height: number;
  };
  viewport: {
    x: number;
    y: number;
    width: number;
    height: number;
  };
  element: {
    x: number;
    y: number;
    width: number;
    height: number;
  };
}

export function focus(id: string): void | Error {
  return withDomNode(id, (el) => el.focus());
}

export function blur(id: string): void | Error {
  return withDomNode(id, (el) => el.blur());
}

export function getViewportOf(id: string): Viewport | Error {
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

export interface SetViewport {
  x: number;
  y: number;
}

export function setViewport(options: SetViewport): void {
  window.scroll(options.y, options.y);
}

export interface SetViewportOf {
  id: string;
  x: number;
  y: number;
}

export function setViewportOf(options: SetViewportOf): void | Error {
  return withDomNode(options.id, (el) => {
    el.scrollLeft = options.x;
    el.scrollTop = options.y;
  });
}

export function getElement(id: string): DomElement | Error {
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

function withDomNode<a>(id: string, callback: (HTMLElement) => a): a | Error {
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
