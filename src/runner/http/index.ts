// Http Task

export interface Request {
  url: string;
  method: string;
  headers: [name: string, value: string][];
  expect: Expect;
  timeout: number | null;
  body: any;
}

export type Response = ResponseSuccess | ResponseError;
export type Expect = "STRING" | "JSON" | "WHATEVER";

export interface ResponseSuccess {
  body: any;
  url: string;
  headers: { [header: string]: string };
  status: number;
  statusText: string;
}

export type HttpError =
  | "BAD_URL"
  | "NETWORK_ERROR"
  | "TIMEOUT"
  | "BAD_BODY"
  | string;

export interface ResponseError {
  error: { reason: HttpError; message: string };
}
