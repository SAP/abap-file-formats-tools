import * as fs from 'fs';
import * as path from 'path';
import {initializeABAP} from "../output/init.mjs";
await initializeABAP();

async function run() {
  const result = await abap.Classes["CL_RUN"].run();
  if (fs.existsSync("generated") === false) {
    fs.mkdirSync("generated");
  }
  for (const row of result.array()) {
    fs.writeFileSync("generated" + path.sep + row.get().filename.get(), row.get().contents.get());
  }
}

run();