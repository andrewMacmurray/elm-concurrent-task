// Http Task

export interface Request {
  url: string;
  method: string;
  headers: { name: string; value: string }[];
  body: any;
}

export type Response = ResponseSuccess | ResponseError;

export interface ResponseSuccess {
  body: any;
  status: number;
  statusText: string;
}

export type HttpError = "BAD_URL" | "NETWORK_ERROR" | "TIMEOUT" | "UNKNOWN";

export interface ResponseError {
  error: HttpError;
  body: any;
  status: number;
  statusText: string;
}
