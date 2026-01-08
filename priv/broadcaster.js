let ws = null;
let audioContext = null;
let analyser = null;
let microphone = null;
let processor = null;
let chunkCount = 0;
let totalBytes = 0;
let isRecording = false;

const startBtn = document.getElementById('startBtn');
const stopBtn = document.getElementById('stopBtn');
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
    document.getElementById('chunksSent').textContent = chunkCount;
    document.getElementById('dataSent').textContent = 
        (totalBytes / 1024).toFixed(2) + ' KB';
}

function updateStatus(text, className) {
    statusDiv.textContent = text;
    statusDiv.className = `status ${className}`;
}

function connectWebSocket() {
    return new Promise((resolve, reject) => {
        log('Connecting to WebSocket...', 'info');
        
        ws = new WebSocket('ws://localhost:9000/ws');
        
        ws.onopen = () => {
            log('✓ WebSocket connected successfully', 'success');
            document.getElementById('wsStatus').textContent = 'Connected';
            updateStatus('🟢 Connected', 'connected');
            resolve();
        };

        ws.onerror = (error) => {
            log('✗ WebSocket error: ' + error, 'error');
            document.getElementById('wsStatus').textContent = 'Error';
            reject(error);
        };

        ws.onclose = () => {
            log('WebSocket connection closed', 'warning');
            document.getElementById('wsStatus').textContent = 'Closed';
            updateStatus('⚫ Disconnected', 'disconnected');
        };
    });
}

async function startBroadcasting() {
    try {
        startBtn.disabled = true;
        log('=== Starting Broadcasting ===', 'info');

        // Connect WebSocket first
        await connectWebSocket();

        // Request microphone access
        log('Requesting microphone access...', 'info');
        const stream = await navigator.mediaDevices.getUserMedia({
            audio: {
                echoCancellation: true,
                noiseSuppression: true,
                autoGainControl: false,
                sampleRate: 48000
            }
        });
        log('✓ Microphone access granted', 'success');

        // Setup audio context
        audioContext = new (window.AudioContext || window.webkitAudioContext)();
        const sampleRate = audioContext.sampleRate;
        document.getElementById('sampleRate').textContent = sampleRate + ' Hz';
        log(`Audio context sample rate: ${sampleRate} Hz`, 'info');

        // Setup analyser for visualization
        analyser = audioContext.createAnalyser();
        analyser.fftSize = 2048;
        
        microphone = audioContext.createMediaStreamSource(stream);
        
        // Create script processor for raw audio data
        const bufferSize = 4096;
        processor = audioContext.createScriptProcessor(bufferSize, 1, 1);
        
        log(`Using buffer size: ${bufferSize} samples`, 'info');
        log(`Chunk duration: ${(bufferSize / sampleRate * 1000).toFixed(2)}ms`, 'info');

        processor.onaudioprocess = (e) => {
            if (!isRecording) return;

            const inputData = e.inputBuffer.getChannelData(0);
            
            // Convert Float32Array to Int16Array (PCM)
            const pcmData = new Int16Array(inputData.length);
            for (let i = 0; i < inputData.length; i++) {
                const s = Math.max(-1, Math.min(1, inputData[i]));
                pcmData[i] = s < 0 ? s * 0x8000 : s * 0x7FFF;
            }

            // Convert to base64
            const uint8Array = new Uint8Array(pcmData.buffer);
            const base64 = btoa(String.fromCharCode.apply(null, uint8Array));
            
            chunkCount++;
            totalBytes += base64.length;
            updateStats();

            const message = JSON.stringify({
                type: 'audio',
                audio: base64,
                sampleRate: sampleRate,
                channels: 1,
                format: 'pcm16'
            });

            if (ws && ws.readyState === WebSocket.OPEN) {
                ws.send(message);
                
                if (chunkCount % 10 === 0) { // Log every 10 chunks
                    log(`→ Sent chunk #${chunkCount} (${base64.length} bytes)`, 'success');
                }
            }
        };

        // Connect audio nodes
        microphone.connect(analyser);
        microphone.connect(processor);
        processor.connect(audioContext.destination);

        isRecording = true;

        // Start visualization
        visualize();

        log('✓ Broadcasting started successfully', 'success');
        log('  Sending raw PCM audio data', 'info');
        updateStatus('🔴 Broadcasting', 'broadcasting');
        startBtn.classList.add('hidden');
        stopBtn.classList.remove('hidden');

    } catch (error) {
        log('✗ Error starting broadcast: ' + error.message, 'error');
        console.error(error);
        startBtn.disabled = false;
    }
}

function stopBroadcasting() {
    log('=== Stopping Broadcasting ===', 'info');
    
    isRecording = false;
    
    if (processor) {
        processor.disconnect();
        log('Processor disconnected', 'info');
    }

    if (microphone) {
        microphone.disconnect();
        log('Microphone disconnected', 'info');
    }

    if (audioContext) {
        audioContext.close();
        log('Audio context closed', 'info');
    }

    if (ws) {
        ws.close();
        log('WebSocket closed', 'info');
    }

    log('✓ Broadcasting stopped', 'success');
    updateStatus('⚫ Disconnected', 'disconnected');
    stopBtn.classList.add('hidden');
    startBtn.classList.remove('hidden');
    startBtn.disabled = false;
}

function visualize() {
    if (!isRecording) return;

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
        gradient.addColorStop(0, '#4CAF50');
        gradient.addColorStop(1, '#81C784');
        
        canvasCtx.fillStyle = gradient;
        canvasCtx.fillRect(x, canvas.height - barHeight, barWidth, barHeight);

        x += barWidth + 1;
    }
}

startBtn.addEventListener('click', startBroadcasting);
stopBtn.addEventListener('click', stopBroadcasting);

log('Page loaded - Ready to broadcast', 'info');
log('  Using raw PCM16 audio format', 'info');