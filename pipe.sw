// MAIN
	import AVFoundation
	let url = URL(fileURLWithPath: "core.mov")
	let asset = AVAsset(url: url)
	let duration = try await asset.load(.duration)
	// let metadata = try await asset.load(.metadata)
	// let audio = try await asset.loadTracks(withMediaType: .audio)
	let video = try await asset.loadTracks(withMediaType: .video)
	let track = video[0]
	let rate = try await track.load(.nominalFrameRate)
	let length = try await track.load(.totalSampleDataLength)
	let settings = [String(kCVPixelBufferPixelFormatTypeKey):kCVPixelFormatType_422YpCbCr8]
	let output = AVAssetReaderTrackOutput(track: track, outputSettings: settings); output.alwaysCopiesSampleData = false
	let reader = try! AVAssetReader(asset: asset); reader.add(output)
	reader.startReading()
	var count = 0
	var total = 0
	var maybe = output.copyNextSampleBuffer()
	while (maybe != nil) {
	let buffer = maybe!
	try! buffer.makeDataReady()
	let image = buffer.imageBuffer!
	CVPixelBufferLockBaseAddress(image, CVPixelBufferLockFlags(rawValue: 0))
	let bytesPerRow = CVPixelBufferGetBytesPerRow(image)
	let height = CVPixelBufferGetHeight(image)
	let src_buff = CVPixelBufferGetBaseAddress(image)
	let data = NSData(bytes: src_buff, length: bytesPerRow * height)
	total = total + data.count
	count = count + 1
	CVPixelBufferUnlockBaseAddress(image, CVPixelBufferLockFlags(rawValue: 0))
	maybe = output.copyNextSampleBuffer()}
	print("duration:\(duration.seconds) length:\(length) rate:\(rate) count:\(count) total:\(total) count/rate:\(Float(count)/rate) total/count:\(total/count)")