import * as fs from 'fs';
import {initializeABAP} from "../output/init.mjs";
await initializeABAP();

async function run() {
  const result = await abap.Classes["CL_RUN"].run();
//  console.dir(result.get());
  fs.writeFileSync("out.json", result.get());
}

run();