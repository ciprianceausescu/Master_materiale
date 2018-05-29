package main;

/**
 * Created by Ciprian Mihai on 4/01/2018.
 */

import javax.swing.JComponent;
import java.awt.Graphics;
import java.awt.Image;
//Clasă care crează un ImagePanel în care va fi poziționată imaginea de fundal a intefeței grafice
class ImagePanel extends JComponent {
    private Image image;
    public ImagePanel(Image image) {
        this.image = image;
    }
    @Override
    protected void paintComponent(Graphics g) {
        super.paintComponent(g);
        g.drawImage(image, 0, 70, this);
    }
}