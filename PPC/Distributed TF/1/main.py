import tensorflow as tf


cluster = tf.train.ClusterSpec({"local": ["localhost:2222", "localhost:2223"]})

x = tf.constant(20)

with tf.device("/job:local/task:1"):
    y2 = x - 10
	
with tf.device("/job:local/task:0"):
    y1 = x + 10
    y = y1 + y2

with tf.Session("grpc://localhost:2222") as sess:
	result = sess.run(y)
	print("y1 = " + str(y1) + ". Tensor-ul are valoarea: " + str(y1.eval()))
	print("y2 = " + str(y2) + ". Tensor-ul are valoarea: " + str(y2.eval()))
	print("Rezultatul este " + str(result) + ".")
