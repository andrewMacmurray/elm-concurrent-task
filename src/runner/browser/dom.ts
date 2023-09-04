export function focus(id: string): void | { error: string } {
  const el = document.getElementById(id);
  if (el) {
    return el.focus();
  }
  return { error: id };
}

export function blur(id: string): void | { error: string } {
  const el = document.getElementById(id);
  if (el) {
    return el.blur();
  }
  return { error: id };
}
