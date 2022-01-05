const process = require("process");
const mqtt = require('mqtt')
const options = {
    port: 1883,
    host: "localhost",
    clientId: 'mqttjs_' + Math.random().toString(16).substr(2, 8),
    keepalive: 60,
    reconnectPeriod: 1000,
    protocol: 'mqtt',
    protocolId: 'MQIsdp',
    protocolVersion: 3,
    clean: true,
    encoding: 'utf8'
};

const messageCount = 1000000;
let previous = 0;

const client = mqtt.connect(process.env.MQTT_HOST, options);

client.on('connect', function () {
    client.subscribe(`test/topic`, function (err) {});
    for (let i = 0; i <= messageCount; i++) {
        client.publish("test/topic", i.toString())
    }
})

client.on('message', function (topic, message) {
    const current = Number(message.toString());
    if (current >= 1 && (current !== (previous + 1))) {
        client.end();
        console.log(previous + " -> " + current);
        process.exit(1);
    }

    previous = current;

    if (current == messageCount) {
        client.end();
        console.log("all messages were processed in order");
        process.exit(0);
    }
})

setInterval(() => {
    console.log(Math.round(previous * 100 / messageCount) + "% ...");
}, 100);