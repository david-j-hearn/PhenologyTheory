
import java.awt.Color;
import java.util.ArrayList;
import java.util.Hashtable;
import java.util.Random;
import javax.swing.JOptionPane;
import javax.swing.SwingUtilities;

/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/Classes/Class.java to edit this template
 */
/**
 *
 * @author David
 */
public class Headless_KMeans {

    // rgb array is the array of pixel colors from an image
    // k is the number of clusters
    public static int[] trainKMeans(int[] rgb, int k, boolean convert) {

        // Frame_ProgressBar pb = new Frame_ProgressBar("Training pixel classifier");
        // pb.setVisible(true);
        int maxIts = 500;
        Random random = new Random();
        if (rgb.length < k) {
            System.out.println("pixel length<k!!!");
            return null;
        }

        int[] nR = new int[k];
        int[] nG = new int[k];
        int[] nB = new int[k];
        int[] mean = new int[k];
        int[] mean1 = new int[k];
        int[] total = new int[k];
        int[] cluster = new int[rgb.length];
        // Initially assigning random centers for k clusters
        for (int i = 0; i < k; i++) {
            mean1[i] = rgb[random.nextInt(rgb.length)];
        }

        Color[] colors = new Color[rgb.length];
        for (int i = 0; i < rgb.length; i++) {
            colors[i] = new Color(rgb[i]);
        }

        int cntIts = 0;

        // Thread t = new Thread( new Runnable() {
        //@Override
        //public void run() {
        int center = 0;
        do {
            //System.out.println("On K-Means iteration " + (cntIts++));
            //int progress = (int) (100.0 * (double) cntIts / (double) maxIts);
            //pb.setProgress(progress);

            for (int i = 0; i < mean1.length; i++) {
                mean[i] = mean1[i];
                total[i] = nR[i] = nG[i] = nB[i] = 0;
            }
            // Finding closest center
            for (int i = 0; i < rgb.length; i++) {
                double thresholdDist = Double.MAX_VALUE;
                Color d = colors[i];
                for (int j = 0; j < mean1.length; j++) {

                    Color e = new Color(mean1[j]);
                    int dR = d.getRed() - e.getRed();
                    int dG = d.getGreen() - e.getGreen();
                    int dB = d.getBlue() - e.getBlue();
                    double dist = Math.sqrt(dR * dR + dG * dG + dB * dB);
                    if (dist < thresholdDist) {
                        thresholdDist = dist;
                        center = j;
                    }
                }
                cluster[i] = center;
                total[center]++;
                nR[center] += d.getRed();
                nG[center] += d.getGreen();
                nB[center] += d.getBlue();
            }
            // set center values
            for (int i = 0; i < k; i++) {
                int aR = findAverage(nR[i], total[i]);
                int aG = findAverage(nG[i], total[i]);
                int aB = findAverage(nB[i], total[i]);
                mean1[i] = (new Color(aR, aG, aB)).getRGB();
                //((aR & 0x000000FF) << 16) | ((aG & 0x000000FF) << 8) | ((aB & 0x000000FF));
            }
        } while (!converge(mean, mean1) && cntIts <= maxIts);
        // }
        //});

        //t.start();
        //compress original image
        if (convert) {
            for (int i = 0; i < rgb.length; i++) {
                rgb[i] = mean1[cluster[i]];
            }
        }

        //pb.setProgress(100);
        return (mean);
    }

    public static Hashtable<Integer, ArrayList<Integer>> classifyKMeans(int[] means, int[] rgb, boolean setPixels) {

        // Frame_ProgressBar pb = new Frame_ProgressBar("Classifying pixels");
        int k = means.length;
        int size = rgb.length;
        int[] cluster = new int[size];
        Hashtable<Integer, ArrayList<Integer>> classification = new Hashtable<Integer, ArrayList<Integer>>();
        try {

            for (int i = 0; i < k; i++) {
                classification.put(i, new ArrayList<Integer>());
            }
        } catch (Exception e) {
            JOptionPane.showMessageDialog(null,"Clustering of pixels failed. Try a different image.");
        }

        Color[] colors = new Color[size];
        for (int i = 0; i < size; i++) {
            colors[i] = new Color(rgb[i]);
        }

        Color[] centers = new Color[k];
        for (int i = 0; i < k; i++) {
            centers[i] = new Color(means[i]);
        }

        for (int i = 0; i < size; i++) {
            //int progress = (int) (100.0 * (double) i / (double) size);
            //pb.setProgress(progress);
            /*
            final int progress = (int) (100.0 * (double) i / (double) size);

            try {
                SwingUtilities.invokeLater(new Runnable() {
                    public void run() {
                        pb.setProgress(progress);
                    }
                });
            } catch (Exception e) {
                //JOptionPane.showMessageDialog(frame, e.getMessage());
            }
             */

            double thresholdDist = Double.MAX_VALUE;
            Color d = colors[i];
            int center = -1;
            for (int j = 0; j < k; j++) {
                Color e = centers[j];
                int dR = d.getRed() - e.getRed();
                int dG = d.getGreen() - e.getGreen();
                int dB = d.getBlue() - e.getBlue();
                double dist = Math.sqrt(dR * dR + dG * dG + dB * dB);
                if (dist < thresholdDist) {
                    thresholdDist = dist;
                    center = j;
                    cluster[i] = means[j];
                }
            }
            classification.get(center).add(i);
        }
        //pb.setProgress(100);
        if (setPixels) {
            System.arraycopy(cluster, 0, rgb, 0, size);
        }
        return classification;
    }

    private static int findAverage(double s, double k) {
        int a = (int) (s / k);
        return a;
    }

    private static boolean converge(int[] mean, int[] mean1) {
        for (int i = 0; i < mean1.length; i++) {
            if (mean[i] != mean1[i]) {
                return false;
            }
        }

        return true;
    }

    //classify
    //Input:
    //  the k int RGBs of the trained cluster centers,
    //  int array of unclassified RBGs of pixels,
    //return:
    //  hashtable of integer category (one of k possible values), array list of integer pixel point localities
}
