// Run after transpilation: node test/conditions-in.mjs
import {mkdirSync, writeFileSync} from 'node:fs';
import {initializeABAP} from '../output/init.mjs';

await initializeABAP();
mkdirSync('generated', {recursive: true});
for (const type of ['ty_cond_in', 'ty_cond_in_nested']) {
  const writer = await new abap.Classes.ZCL_AFF_WRITER_JSON_SCHEMA().constructor_({
    schema_id: new abap.types.String().set(`urn:aff:test:${type}`),
  });
  const generator = await new abap.Classes.ZCL_AFF_GENERATOR().constructor_({
    writer: new abap.types.ABAPObject().set(writer),
  });
  const output = await generator.zif_aff_generator$generate_type({
    data: abap.Classes.ZCL_AFF_TEST_TYPES[type].clone(),
  });
  const schema = output.array().map(line => line.get()).join('\n');
  JSON.parse(schema);
  writeFileSync(`generated/${type}.schema.json`, schema);
}
