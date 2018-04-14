package main;

import javax.swing.JComponent;
import java.awt.Graphics;
import java.awt.Image;

class ImagePanel extends JComponent {
    private Image image;
    ImagePanel(Image image) {
        this.image = image;
    }
    @Override
    protected void paintComponent(Graphics g) {
        super.paintComponent(g);
        g.drawImage(image, 0, 0, this);
    }
}