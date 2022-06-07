#!/bin/env python3
import json, uuid, time
print(json.dumps(dict(success="ok", uid=str(uuid.uuid4()), delay=time.sleep(2))))
