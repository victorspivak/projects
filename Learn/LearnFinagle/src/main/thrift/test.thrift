namespace svl.finagle.thrift.test

exception SomeException {
    1: string message
    2: i32 value
}

const i32 MyTestServicePort = 8001

struct JobSpec
{
  1: optional i32 version,
  2: optional string job_id,
  3: optional string request_id,
  4: string job_type,
  5: string params
}

service MyTestService {
  list<byte> fetchBlob(1: i64 id, 2: i32 id2);
  string fetchString(1: string id, 2: optional string msg);
  void exceptUnhandled();
  void exceptHandled() throws (1: SomeException ex);
}