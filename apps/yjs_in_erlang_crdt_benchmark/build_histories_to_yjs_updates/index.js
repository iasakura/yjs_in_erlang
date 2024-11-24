const Y = require("yjs");
const fs = require("fs");
const assert = require("assert");
const zlib = require("zlib");
const path = require("path");
const encoding = require("lib0/encoding");
const {
  Worker,
  isMainThread,
  parentPort,
  workerData,
} = require("worker_threads");

const build = (filename, outputFileName) => {
  let encoder = encoding.createEncoder();

  if (filename == null) {
    console.error(`Usage: $ node check.js file.json[.gz]`);
    process.exit(1);
  }

  const { startContent, endContent, txns } = JSON.parse(
    filename.endsWith(".gz")
      ? zlib.gunzipSync(fs.readFileSync(filename)).toString("utf-8")
      : fs.readFileSync(filename, "utf-8")
  );

  const ydoc = new Y.Doc();
  const content = ydoc.getText("content");
  content.insert(0, startContent);
  const bin = Y.encodeStateAsUpdate(ydoc);
  encoding.writeVarUint8Array(encoder, bin);

  console.log("applying", txns.length, "txns...");
  console.time("apply");
  let prevStateVector = Y.encodeStateVector(ydoc);
  for (let i = 0; i < txns.length; i++) {
    if (i % 10000 == 0) {
      console.log(i);
      // 追記
      fs.appendFileSync(outputFileName, encoding.toUint8Array(encoder));
      encoder = encoding.createEncoder();
    }
    const { patches } = txns[i];
    assert(patches != null);
    assert(patches.length > 0);

    for (const [pos, delHere, insContent] of patches) {
      assert(content.length >= pos + delHere);

      if (delHere !== 0) {
        content.delete(pos, delHere);
        let update = Y.encodeStateAsUpdate(ydoc, prevStateVector);
        encoding.writeVarUint8Array(encoder, update);
        prevStateVector = Y.encodeStateVector(ydoc);
      }

      if (insContent !== "") {
        content.insert(pos, insContent);
        let update = Y.encodeStateAsUpdate(ydoc, prevStateVector);
        encoding.writeVarUint8Array(encoder, update);
        prevStateVector = Y.encodeStateVector(ydoc);
      }
    }
  }
  console.timeEnd("apply");

  assert.strictEqual(content.toString(), endContent);

  console.log(`Looking good - ${txns.length} apply cleanly.`);

  fs.appendFileSync(
    outputFileName,
    Buffer.from(encoding.toUint8Array(encoder))
  );
};

const buildAll = () => {
  if (isMainThread) {
    // Main thread logic
    const jobs = JSON.parse(fs.readFileSync("files.json", "utf-8"));
    const numThreads = jobs.length; // Number of jobs to process
    const workers = [];

    jobs.forEach((job, index) => {
      const worker = new Worker(__filename, {
        workerData: job,
      });
      workers.push(worker);

      // Listen for results from worker
      worker.on("message", (result) => {
        console.log(`Result from worker ${index}:`, result);
      });

      // Handle errors
      worker.on("error", (err) => {
        console.error(`Worker ${index} error:`, err);
      });

      // Worker exit
      worker.on("exit", (msg) => {
        if (msg !== 0) {
          console.error(`Worker ${job} stopped: ${msg}`);
        }
      });
    });
  } else {
    const file = workerData;
    const outputFileName = path.basename(file).replace("json.gz", "update.bin");
    console.log(`Building ${file} to ${outputFileName}`);
    build(file, outputFileName);
    parentPort?.postMessage(`Built ${file} to ${outputFileName}`);
  }
};

buildAll();
