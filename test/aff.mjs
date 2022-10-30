import * as fs from 'fs';
import * as path from 'path';
import {initializeABAP} from "../output/init.mjs";
await initializeABAP();

async function run() {
  if (fs.existsSync("generated") === false) {
    fs.mkdirSync("generated");
  }

  const types = [];
  for (const f of fs.readdirSync("abap-file-formats/file-formats/")) {
    if (f.length === 4) {
      types.push(f.toUpperCase());
    }
  }

  for (const type of types) {
    console.log(type);
    if (type === "ENHO") {
      console.log("\tskip, https://github.com/SAP/abap-file-formats/issues/409");
      continue;
    }

    const result = await abap.Classes["CL_RUN"].run({object_type: new abap.types.String().set(type)});
    const filename = "generated" + path.sep + type.toLowerCase() + "-v1.json";
    fs.writeFileSync(filename, result.get());

// do diffs for all types, after https://github.com/SAP/abap-file-formats/issues/410
//    "diff generated/aobj-v1.json abap-file-formats/file-formats/aobj/aobj-v1.json"
  }

  /*
  const result = await abap.Classes["CL_RUN"].run({object_type: new abap.types.String().set("INTF")});
  fs.writeFileSync("generated" + path.sep + "intf-v1.json", result.get());
  */
}

run();