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

const domNodeError = { error: null };

export function focus(id: string): void | Error {
  const el = document.getElementById(id);
  if (el) {
    return el.focus();
  }
  return domNodeError;
}

export function blur(id: string): void | Error {
  const el = document.getElementById(id);
  if (el) {
    return el.blur();
  }
  return domNodeError;
}

export function getViewportOf(id: string): Viewport | Error {
  const el = document.getElementById(id);
  if (el) {
    return {
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
    };
  }
  return domNodeError;
}
