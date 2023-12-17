// Http Task

export interface HttpRequest {
  url: string;
  method: string;
  headers: [name: string, value: string][];
  expect: Expect;
  timeout: number | null;
  body: string | null;
}

export type HttpResponse = ResponseSuccess | ResponseError;
export type Expect = "STRING" | "JSON" | "BYTES" | "WHATEVER";

export interface ResponseSuccess {
  body: string | null;
  url: string;
  headers: { [header: string]: string };
  statusCode: number;
  statusText: string;
}

export type HttpError = "BAD_URL" | "NETWORK_ERROR" | "TIMEOUT";

export interface ResponseError {
  error: { reason: HttpError; message: string };
}
