package ex2;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.TreeMap;

public class Procesator {
	private String path;

	public Procesator(String path) {
		this.path = path;
	}

	public void proceseaza() throws IOException {
		Path p = Paths.get(path);
		String text = new String(Files.readAllBytes(p));
		String words[] = text.split(" ");
		TreeMap<String, Integer> treeMap = new TreeMap<>();
		for (String word : words) {
			if (treeMap.containsKey(word)) {
				treeMap.put(word, treeMap.get(word) + 1);
			} else {
				treeMap.put(word, 1);
			}
		}
		treeMap.forEach((k, v) -> System.out.println(k + " " + v));
	}

	public static void main(String[] args) throws IOException {
		Procesator procesator = new Procesator("input.txt");
		procesator.proceseaza();
	}
}
