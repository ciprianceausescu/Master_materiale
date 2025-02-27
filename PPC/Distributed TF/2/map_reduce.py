import numpy as np
import tensorflow as tf

cluster = tf.train.ClusterSpec({"local": ["localhost:2222", "localhost:2223"]})

x = tf.placeholder(tf.float32, 100)


with tf.device("/job:local/task:1"):
    first_batch = tf.slice(x, [0], [50])
    mean1 = tf.reduce_mean(first_batch)

with tf.device("/job:local/task:0"):
    second_batch = tf.slice(x, [50], [-1])
    mean2 = tf.reduce_mean(second_batch)
    mean = (mean1 + mean2) / 2


with tf.Session("grpc://localhost:2222") as sess:
	result = sess.run(mean, feed_dict={x: np.random.random(100)})
	#result = sess.run(mean, feed_dict={x: np.ones(100)})
	print(result)
