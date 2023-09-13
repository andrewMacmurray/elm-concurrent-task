export type DomError = { error: null };

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

export interface SetViewportOptions {
  x: number;
  y: number;
}

export interface SetViewportOfOptions {
  id: string;
  x: number;
  y: number;
}
