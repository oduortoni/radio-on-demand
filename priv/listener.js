let ws = null;
let audioContext = null;
let analyser = null;
let chunkCount = 0;
let totalBytes = 0;
let isConnected = false;
let nextStartTime = 0;

const connectBtn = document.getElementById('connectBtn');
const disconnectBtn = document.getElementById('disconnectBtn');
const statusDiv = document.getElementById('status');
const debugLog = document.getElementById('debugLog');

// Visualizer setup
const canvas = document.getElementById('visualizer');
const canvasCtx = canvas.getContext('2d');
canvas.width = canvas.offsetWidth;
canvas.height = canvas.offsetHeight;

function log(message, type = 'info') {
    const timestamp = new Date().toLocaleTimeString();
    const entry = document.createElement('div');
    entry.className = `log-entry log-${type}`;
    entry.textContent = `[${timestamp}] ${message}`;
    debugLog.appendChild(entry);
    debugLog.scrollTop = debugLog.scrollHeight;
    console.log(`[${type.toUpperCase()}]`, message);
}

function clearLog() {
    debugLog.innerHTML = '';
    log('Log cleared', 'info');
}

function updateStats() {
    document.getElementById('chunksReceived').textContent = chunkCount;
    document.getElementById('dataReceived').textContent =
        (totalBytes / 1024).toFixed(2) + ' KB';

    if (audioContext && nextStartTime > 0) {
        const latency = ((nextStartTime - audioContext.currentTime) * 1000).toFixed(0);
        document.getElementById('latency').textContent = latency + ' ms';
    }
}

function updateStatus(text, className) {
    statusDiv.textContent = text;
    statusDiv.className = `status ${className}`;
}

async function initAudioContext() {
    audioContext = new (window.AudioContext || window.webkitAudioContext)();

    // Setup analyser for visualization
    analyser = audioContext.createAnalyser();
    analyser.fftSize = 2048;
    analyser.connect(audioContext.destination);

    // Initialize start time
    nextStartTime = audioContext.currentTime;

    log('✓ Audio context initialized', 'success');
    log(`  Sample rate: ${audioContext.sampleRate} Hz`, 'info');

    // Resume context if suspended
    if (audioContext.state === 'suspended') {
        await audioContext.resume();
        log('  Audio context resumed', 'info');
    }
}

async function connectListener() {
    try {
        connectBtn.disabled = true;
        log('=== Connecting to Radio ===', 'info');

        // Initialize audio context with user gesture
        await initAudioContext();

        // Connect WebSocket
        log('Connecting to WebSocket...', 'info');
        ws = new WebSocket('ws://localhost:9000/ws');

        ws.onopen = () => {
            log('✓ WebSocket connected successfully', 'success');
            document.getElementById('wsStatus').textContent = 'Connected';
            updateStatus('🟢 Listening', 'listening');
            connectBtn.classList.add('hidden');
            disconnectBtn.classList.remove('hidden');
            isConnected = true;
            visualize();
        };

        ws.onerror = (error) => {
            log('✗ WebSocket error', 'error');
            document.getElementById('wsStatus').textContent = 'Error';
            connectBtn.disabled = false;
        };

        ws.onclose = () => {
            log('WebSocket connection closed', 'warning');
            document.getElementById('wsStatus').textContent = 'Closed';
            updateStatus('⚫ Disconnected', 'disconnected');
            isConnected = false;
        };

        ws.onmessage = async (event) => {
            chunkCount++;

            if (chunkCount % 10 === 0) {
                log(`← Received chunk #${chunkCount}`, 'success');
            }

            try {
                const data = JSON.parse(event.data);

                // DEBUG: Log the actual data structure
                if (chunkCount === 1) {
                    log(`  Data keys: ${Object.keys(data).join(', ')}`, 'info');
                    log(`  Type: ${data.type}`, 'info');
                    log(`  Has audio: ${!!data.audio}`, 'info');
                    log(`  SampleRate: ${data.sampleRate}`, 'info');
                    log(`  Channels: ${data.channels}`, 'info');
                    log(`  Format: ${data.format}`, 'info');
                }

                if (data.type === 'audio' && data.audio) {
                    totalBytes += data.audio.length;
                    updateStats();

                    await playAudioChunk(data);
                } else {
                    log(`✗ Missing required fields`, 'error');
                }
            } catch (e) {
                log('✗ Error: ' + e.message, 'error');
                console.error(e);
            }
        };

    } catch (error) {
        log('✗ Error connecting: ' + error.message, 'error');
        connectBtn.disabled = false;
    }
}

async function playAudioChunk(data) {
    try {
        const { audio, sampleRate, channels, format } = data;

        // Validate input
        if (!audio || !sampleRate || !channels) {
            log('✗ Invalid audio data received', 'error');
            return;
        }

        if (chunkCount === 1) {
            log(`  Stream info: ${sampleRate}Hz, ${channels}ch, ${format}`, 'info');
        }

        // Decode base64 to ArrayBuffer
        const binaryString = atob(audio);
        const bytes = new Uint8Array(binaryString.length);
        for (let i = 0; i < binaryString.length; i++) {
            bytes[i] = binaryString.charCodeAt(i);
        }

        // Convert Int16Array to Float32Array
        const pcmData = new Int16Array(bytes.buffer);
        const numSamples = pcmData.length;

        // Validate we have samples
        if (numSamples === 0) {
            log('✗ No audio samples in chunk', 'error');
            return;
        }

        const floatData = new Float32Array(numSamples);
        for (let i = 0; i < numSamples; i++) {
            floatData[i] = pcmData[i] / (pcmData[i] < 0 ? 0x8000 : 0x7FFF);
        }

        // Create audio buffer with explicit sample rate
        const audioBuffer = audioContext.createBuffer(
            channels,
            numSamples,
            sampleRate
        );

        // Copy data to buffer
        audioBuffer.getChannelData(0).set(floatData);

        // Create source and schedule playback
        const source = audioContext.createBufferSource();
        source.buffer = audioBuffer;
        source.connect(analyser);

        const currentTime = audioContext.currentTime;
        const startTime = Math.max(currentTime, nextStartTime);

        source.start(startTime);
        nextStartTime = startTime + audioBuffer.duration;

        if (chunkCount % 10 === 0) {
            log(`  ✓ Playing ${audioBuffer.duration.toFixed(3)}s`, 'success');
        }

    } catch (e) {
        log(`✗ Playback error: ${e.message}`, 'error');
        console.error('Full error:', e);
    }
}

function visualize() {
    if (!isConnected) return;

    requestAnimationFrame(visualize);

    const bufferLength = analyser.frequencyBinCount;
    const dataArray = new Uint8Array(bufferLength);
    analyser.getByteFrequencyData(dataArray);

    canvasCtx.fillStyle = '#1e1e1e';
    canvasCtx.fillRect(0, 0, canvas.width, canvas.height);

    const barWidth = (canvas.width / bufferLength) * 2.5;
    let x = 0;

    for (let i = 0; i < bufferLength; i++) {
        const barHeight = (dataArray[i] / 255) * canvas.height;

        const gradient = canvasCtx.createLinearGradient(0, canvas.height - barHeight, 0, canvas.height);
        gradient.addColorStop(0, '#2196F3');
        gradient.addColorStop(1, '#64B5F6');

        canvasCtx.fillStyle = gradient;
        canvasCtx.fillRect(x, canvas.height - barHeight, barWidth, barHeight);

        x += barWidth + 1;
    }
}

function disconnect() {
    log('=== Disconnecting ===', 'info');

    isConnected = false;

    if (ws) {
        ws.close();
    }

    if (audioContext) {
        audioContext.close();
    }

    log('✓ Disconnected', 'success');
    disconnectBtn.classList.add('hidden');
    connectBtn.classList.remove('hidden');
    connectBtn.disabled = false;
}

connectBtn.addEventListener('click', connectListener);
disconnectBtn.addEventListener('click', disconnect);

log('Page loaded - Ready to listen', 'info');
log('⚠ Click "Connect & Listen" to start', 'warning');