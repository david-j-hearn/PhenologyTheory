
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.image.BufferedImage;
import java.awt.Image;
import java.awt.Point;
import java.awt.geom.AffineTransform;
import java.awt.geom.Point2D;
import java.awt.image.ColorModel;
import java.awt.image.DataBuffer;
import java.awt.image.DataBufferInt;
import java.awt.image.Raster;
import java.awt.image.SinglePixelPackedSampleModel;
import java.awt.image.WritableRaster;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.Hashtable;
import java.util.Random;
import java.util.Set;
import java.util.TreeMap;
import java.util.concurrent.ThreadLocalRandom;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.imageio.ImageIO;
import javax.swing.JOptionPane;

/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/GUIForms/JFrame.java to edit this template
 */
/**
 *
 * @author David
 */
public class Frame_DataSampler extends javax.swing.JFrame {

    /**
     * Creates new form NewJFrame
     */
    public Frame_DataSampler() {

        initComponents();
        //readDefaults();
        //this.TextField_ImageNumber.setText("0");

        rng = new Random();
        //this.setSampleNumber();

        //this.fp = new Frame_Parameters(this);
        //OKDialog = new Dialog_OK(this,false,"Saved!");
    }

    private String getColorString(Color c) {
        int r = c.getRed();
        int g = c.getGreen();
        int b = c.getBlue();

        String cString = r + "," + g + "," + b;
        return cString;
    }

    private static Color parseColorString(String cString) {
        if (cString == null) {
            return Color.RED;
        }
        String[] rgb = cString.split(",", 0);
        if (rgb.length != 3) {
            return Color.RED;
        }
        try {
            return new Color(Integer.parseInt(rgb[0]), Integer.parseInt(rgb[1]), Integer.parseInt(rgb[2]));
        } catch (Exception e) {
            return Color.RED;
        }
    }

    private void getSamplingState() {
        fastMode = this.CheckBoxMenuItem_FastMode.isSelected();
        autoSave = this.CheckBoxMenuItem_Autosave.isSelected();
        randomSampling = this.CheckBoxMenuItem_RandomizeSampling.isSelected();
        resetOnSave = this.CheckBox_ResetOnSave.isSelected();
    }

    private void setSamplingState() {
        this.CheckBoxMenuItem_Autosave.setSelected(autoSave);
        this.CheckBoxMenuItem_FastMode.setSelected(fastMode);
        this.CheckBoxMenuItem_RandomizeSampling.setSelected(randomSampling);
        this.CheckBox_ResetOnSave.setSelected(resetOnSave);
    }

    public void writeProjectParametersFile() {
        String defaultFile = this.rootDirectory + separator + this.defaultSamplesDirectory + separator + this.projectName + separator + projectParametersFile;

        getSamplingState();

        try {
            BufferedWriter writer = new BufferedWriter(new FileWriter(defaultFile));
            writer.write("rectangleSelectedColor\t" + getColorString(rectangleSelectedColor) + "\n");
            writer.write("rectangleRandomColor\t" + getColorString(rectangleRandomColor) + "\n");
            writer.write("pointSelectedColor\t" + getColorString(pointSelectedColor) + "\n");
            writer.write("pointRandomColor\t" + getColorString(pointRandomColor) + "\n");
            writer.write("pointZoomColor\t" + getColorString(pointZoomColor) + "\n");
            writer.write("sampledPointsColor\t" + getColorString(sampledPointsColor) + "\n");
            writer.write("defaultImageWidth\t" + defaultImageWidth + "\n");
            writer.write("defaultZoom\t" + defaultZoom + "\n");
            writer.write("defaultWindowSize\t" + defaultWindowSize + "\n");
            writer.write("defaultOverviewSize\t" + defaultOverviewSize + "\n");
            writer.write("defaultLocalSize\t" + defaultLocalSize + "\n");
            writer.write("savedMontageSize\t" + savedMontageSize + "\n");
            writer.write("savedOverviewSize\t" + savedOverviewSize + "\n");
            writer.write("defaultImageFormat\t" + defaultImageFormat + "\n");
            writer.write("defaultImagesDirectory\t" + defaultImagesDirectory + "\n");
            writer.write("defaultCharactersDirectory\t" + defaultCharactersDirectory + "\n");
            writer.write("defaultAttributesDirectory\t" + defaultAttributesDirectory + "\n");
            writer.write("defaultCharactersFile\t" + defaultCharactersFile + "\n");
            writer.write("defaultAttributesFile\t" + defaultAttributesFile + "\n");
            writer.write("defaultSamplesDirectory\t" + defaultSamplesDirectory + "\n");
            writer.write("rootDirectory\t" + rootDirectory + "\n");
            writer.write("mappingFile\t" + mappingFile + "\n");
            writer.write("dataFile\t" + dataFile + "\n");
            writer.write("defaultTrainingPercentage\t" + defaultTrainingPercentage + "\n");
            writer.write("defaultTestingPercentage\t" + defaultTestingPercentage + "\n");
            writer.write("defaultValidationPercentage\t" + defaultValidationPercentage + "\n");
            writer.write("partitionByFile\t" + partitionByFile + "\n");
            writer.write("kDefault\t" + kDefault + "\n");
            writer.write("projectName\t" + projectName + "\n");
            writer.write("user\t" + user + "\n");
            writer.write("imageNumber\t" + imageNumber + "\n");
            writer.write("notesFile\t" + notesFile + "\n");
            writer.write("fastMode\t" + fastMode + "\n");
            writer.write("autoSave\t" + autoSave + "\n");
            writer.write("randomSampling\t" + randomSampling + "\n");
            writer.write("resetOnSave\t" + resetOnSave + "\n");

            writer.close();
        } catch (IOException ex) {
            Logger.getLogger(Frame_DataSampler.class
                    .getName()).log(Level.SEVERE, null, ex);
        }

    }

    public boolean readProjectParametersFile() {
        //check if file exists
        String defaultFile = this.rootDirectory + separator + this.defaultSamplesDirectory + separator + this.projectName + separator + this.projectParametersFile;
        propertiesHash = new Hashtable<String, String>();
        try {
            if (Files.exists(Paths.get(defaultFile))) {
                BufferedReader br = new BufferedReader(new FileReader(defaultFile));
                String line;
                while ((line = br.readLine()) != null) {
                    String[] data = line.split("\\t");
                    if (data.length == 2) {
                        propertiesHash.put(data[0], data[1]);
                    }
                }
                br.close();
                parsePropertiesHash();
                return true;
            } else {
                return false;
            }
        } catch (Exception e) {
            return false;
        }

    }

    private void parsePropertiesHash() {

        rectangleSelectedColor = parseColorString(propertiesHash.get("rectangleSelectedColor"));
        pointSelectedColor = parseColorString(propertiesHash.get("pointSelectedColor"));
        pointRandomColor = parseColorString(propertiesHash.get("pointRandomColor"));
        pointZoomColor = parseColorString(propertiesHash.get("pointZoomColor"));
        sampledPointsColor = parseColorString(propertiesHash.get("sampledPointsColor"));

        defaultImageWidth = Integer.parseInt(propertiesHash.get("defaultImageWidth"));
        defaultZoom = Double.parseDouble(propertiesHash.get("defaultZoom"));

        defaultWindowSize = Integer.parseInt(propertiesHash.get("defaultWindowSize"));
        defaultOverviewSize = Integer.parseInt(propertiesHash.get("defaultOverviewSize"));
        defaultLocalSize = Integer.parseInt(propertiesHash.get("defaultLocalSize"));
        savedMontageSize = Integer.parseInt(propertiesHash.get("savedMontageSize"));
        savedOverviewSize = Integer.parseInt(propertiesHash.get("savedOverviewSize"));

        defaultImageFormat = propertiesHash.get("defaultImageFormat");

        defaultImagesDirectory = propertiesHash.get("defaultImagesDirectory");
        defaultCharactersDirectory = propertiesHash.get("defaultCharactersDirectory");
        defaultAttributesDirectory = propertiesHash.get("defaultAttributesDirectory");
        defaultCharactersFile = propertiesHash.get("defaultCharactersFile");
        defaultAttributesFile = propertiesHash.get("defaultAttributesFile");
        defaultSamplesDirectory = propertiesHash.get("defaultSamplesDirectory");
        rootDirectory = propertiesHash.get("rootDirectory");
        mappingFile = propertiesHash.get("mappingFile");
        dataFile = propertiesHash.get("dataFile");
        notesFile = propertiesHash.get("notesFile");

        defaultTrainingPercentage = Integer.parseInt(propertiesHash.get("defaultTrainingPercentage"));
        defaultTestingPercentage = Integer.parseInt(propertiesHash.get("defaultTestingPercentage"));
        defaultValidationPercentage = Integer.parseInt(propertiesHash.get("defaultValidationPercentage"));

        kDefault = Integer.parseInt(propertiesHash.get("kDefault"));

        projectName = propertiesHash.get("projectName");
        user = propertiesHash.get("user");

        partitionByFile = Boolean.parseBoolean(propertiesHash.get("partitionByFile"));
        fastMode = Boolean.parseBoolean(propertiesHash.get("fastMode"));
        autoSave = Boolean.parseBoolean(propertiesHash.get("autoSave"));
        randomSampling = Boolean.parseBoolean(propertiesHash.get("randomSampling"));
        resetOnSave = Boolean.parseBoolean(propertiesHash.get("resetOnSave"));

        imageNumber = Integer.parseInt(propertiesHash.get("imageNumber"));

    }

//once the first sample of a project is saved, then parameter values for the project should not be altered to assure all samples have same dimensionality
    public void disableParameterEditing() {

        //parameters       
        if (this.fp == null) {
            fp = new Frame_Parameters(this);
        }
        fp.disableParameterEditing();

        //colors
        //if (this.fsc == null) {
        //    fsc = new Frame_SelectionColors(this);
        //}
        //Colors don't influence sample properties!! No need to disable!!
        //fsc.disableColorSecting();
        //text fields
        this.CheckBoxMenuItem_FastMode.setEnabled(false);
        disableSamplingTextFields();
        disableFileSelectionMenuItems();
    }

    private void disableFileSelectionMenuItems() {
        this.MenuItem_OpenAddedAttributeFile.setEnabled(false);
        this.MenuItem_OpenCharactersFile.setEnabled(false);
        this.MenuItem_OpenImagesDirectory.setEnabled(false);

    }

    private void disableSamplingTextFields() {
        TextField_LocalSize.setEnabled(false);
        TextField_OverviewSize.setEnabled(false);
        TextField_WindowSize.setEnabled(false);
    }

    void setProjectCharactersFile() {
        if (characterFile != null) {
            if (characterFile.length() > 0) {
                String charFile = this.rootDirectory + separator + this.defaultCharactersDirectory + separator + characterFile;
                String projCharFile = rootDirectory + separator + defaultSamplesDirectory + separator + projectName + separator + defaultCharactersFile;
                try {
                    if (Files.exists(Paths.get(charFile)) && !Files.exists(Paths.get(projCharFile))) {
                        Files.copy(Paths.get(charFile), Paths.get(projCharFile));
                    }
                } catch (Exception e) {

                }
            }
        }
    }

    void setProjectAttributesFile() {
        if (attributesFile != null) {
            if (attributesFile.length() > 0) {
                String attFile = this.rootDirectory + separator + this.defaultAttributesDirectory + separator + attributesFile;
                String projAttFile = rootDirectory + separator + defaultSamplesDirectory + separator + projectName + separator + defaultAttributesFile;
                try {
                    if (Files.exists(Paths.get(attFile)) && !Files.exists(Paths.get(projAttFile))) {
                        Files.copy(Paths.get(attFile), Paths.get(projAttFile));
                    }
                } catch (Exception e) {

                }
            }
        }
    }

    public void initializeDataFromDefaults() {

        //System.out.println("Welcome ");
        boolean newProject = true;
        checkDirectories();

        if (!readProjectParametersFile()) {
            if (this.fp == null) {
                fp = new Frame_Parameters(this);
            }
            if (this.fsc == null) {
                fsc = new Frame_SelectionColors(this);
            }

            JOptionPane.showMessageDialog(this, "<html>Welcome to Herbarium DNN Sampler.<br>It looks like this is a new project.<br>Be sure to set your project's parameters before saving any sampled data.<br>You can set parameters and colors under the 'Parameters' menu.<br>Once you have saved a sample, you will not be able to change sampling parameters.</html>");
            //fp.setVisible(true);
            //while (!fp.doneSelection) {
            //    try {
            //        Thread.sleep(1000);
            //        if (!fp.isVisible()) {
            //            fp.doneSelection = true;
            //        }
            //    } catch (InterruptedException ex) {
            //Logger.getLogger(Frame_DataSampler.class.getName()).log(Level.SEVERE, null, ex);
            //    }
            //}
            //fp.doneSelection = false;
            //if (this.fsc == null) {
            //     fsc = new Frame_SelectionColors(this);
            // }
            //fsc.setVisible(true);
            //while (!fsc.doneSelection) {
            //    try {
            //        Thread.sleep(1000);
            //        if (!fsc.isVisible()) {
            //            fsc.doneSelection = true;
            //        }
            //    } catch (InterruptedException ex) {
            //Logger.getLogger(Frame_DataSampler.class.getName()).log(Level.SEVERE, null, ex);
            //    }
            //}
            //fsc.doneSelection = false;

        } else {
            newProject = false;
        }

        this.setSamplingState();

        //write file partition mapping, if needed to a file
        checkPriorPartitioningScheme(this.partitionByFile);

        this.setInitialCharacterList();
        this.setInitialAttributesList();
        setProjectCharactersFile();
        setProjectAttributesFile();

        this.setImagesDirectory(this.defaultImagesDirectory, this.rootDirectory, newProject);
        setWindowSizeInformation();

        this.sampleNumber = this.readSampledPixels(true);
        setSampleNumber();
        //There are already saved values. Don't allow editing of parameters to assure that sampled object dimensions remain the same from one session to the next
        if (this.sampleNumber > 0) {
            disableParameterEditing();
        }

        this.checkSaveButton();

        if (newProject) {
            this.setInitialNotes();
        } else {
            this.readNotesFromFile();
        }

        if (this.CheckBoxMenuItem_FastMode.isSelected()) {
            setImageStateHash(true);
        }
    }

    private int getNumberOfCharacters() {
        if (headerHash == null) {
            setHeaderHash();
        }
        return headerHash.size();
    }

    private void setHeaderHash() {
        if (charactersHash == null) {
            JOptionPane.showMessageDialog(this, "Illegally reached 'setHeaderHash' when the characters are null");
            System.exit(0);
        }
        if (charactersHash.size() <= 0) {
            JOptionPane.showMessageDialog(this, "Illegally reached 'setHeaderHash' with no characters");
            System.exit(0);
        }

        headerHash = new Hashtable<Integer, String>();

        int cnt = 0;
        //make the characters ordered in the data sheet
        TreeMap charTreeMap = new TreeMap<String, Boolean>(charactersHash);
        Set<String> keys = charTreeMap.keySet();
        for (String character : keys) {

            headerHash.put(cnt, character);
            cnt++;

        }
    }

    public void setImageStateHash(boolean reinitialize) {
        if (this.CheckBoxMenuItem_FastMode.isSelected()) {
            if (this.imageStateHash == null || reinitialize) {
                this.imageStateHash = new Hashtable<String, String>(5000);
            }

            String directory = rootDirectory + separator + defaultSamplesDirectory + separator + this.projectName;
            BufferedReader reader;

            //Check fastmode points
            String sampleDataFile = directory + separator + dataFile; //not user settable
            setHeaderHash();
            int numChars = getNumberOfCharacters();

            try {
                reader = new BufferedReader(new FileReader(sampleDataFile));
                String line = reader.readLine();
                line = reader.readLine();
                while (line != null) {
                    String[] data = line.split("\\t", 0);
                    String imageFile = data[data.length - 1];
                    String imageCharacters = new String();
                    for (int i = 12; i < 12 + numChars; i++) {
                        if (data[i].equals((String) "1")) {
                            imageCharacters += " " + headerHash.get(i - 12);
                        }
                    }

                    imageStateHash.put(imageFile, imageCharacters);
                    line = reader.readLine();

                }
                reader.close();
            } catch (IOException e) {
                return;
                //System.out.println("No metadata file yet.");
            }
            System.out.println("Done setting image state hash");
        }
    }

    private void readNotesFromFile() {
        String file = rootDirectory + separator + defaultSamplesDirectory + separator + projectName + separator + notesFile;
        try {
            this.notes = Files.readString(Paths.get(file));
        } catch (IOException ex) {
            System.out.println("Could not read notes from file " + file);
        }
    }

    public void setInitialNotes() {
        Date date = Calendar.getInstance().getTime();
        DateFormat dateFormat = new SimpleDateFormat("yyyy-mm-dd hh:mm:ss");
        String strDate = dateFormat.format(date);
        String file = rootDirectory + separator + defaultSamplesDirectory + separator + projectName + separator + notesFile;

        try {
            notes = Files.readString(Paths.get(file));
        } catch (IOException ex) {

        }
        if (notes == null) {
            notes = "Project " + this.projectName + " notes started " + strDate + " for user " + this.user + "\n";
        }

        if (notes.length() == 0) {
            notes = "Project " + this.projectName + " notes started " + strDate + " for user " + this.user + "\n";
        } else {
            notes += "\nSession notes started " + strDate + " for user " + this.user + "\n";
        }

        try {
            Files.writeString(Paths.get(file), notes);
        } catch (IOException ex) {
            System.out.println("Could not write notes to file " + file);
        }
    }

    public void closeSessionNotes() {
        Date date = Calendar.getInstance().getTime();
        DateFormat dateFormat = new SimpleDateFormat("yyyy-mm-dd hh:mm:ss");
        String strDate = dateFormat.format(date);
        String file = rootDirectory + separator + defaultSamplesDirectory + separator + projectName + separator + notesFile;

        try {
            notes = Files.readString(Paths.get(file));
        } catch (IOException ex) {

        }
        if (notes == null) {
            notes = "Project " + this.projectName + " notes ended " + strDate + "\n";
        }

        if (notes.length() == 0) {
            notes = "Project " + this.projectName + " notes ended " + strDate + "\n";
        } else {
            notes += "\nSession notes ended " + strDate + "\n";
        }

        writeNotesToFile(notes);
    }

    public void writeNotesToFile(String notesI) {
        String file = rootDirectory + separator + defaultSamplesDirectory + separator + projectName + separator + notesFile;
        try {
            Files.writeString(Paths.get(file), notesI);
        } catch (IOException ex) {
            System.out.println("Could not write notes to file " + file);
        }
    }

    public void setWindowSizeInformation() {
        if (defaultOverviewSize % 2 == 0) {
            defaultOverviewSize++;
        }
        if (defaultWindowSize % 2 == 0) {
            defaultWindowSize++;
        }
        if (defaultLocalSize % 2 == 0) {
            defaultLocalSize++;
        }

        this.TextField_OverviewSize.setText(String.valueOf(defaultOverviewSize));
        this.TextField_WindowSize.setText(String.valueOf(defaultWindowSize));
        this.TextField_LocalSize.setText(String.valueOf(defaultLocalSize));
        this.TextField_ZoomFactor.setText(String.valueOf(defaultZoom));
        currentZoom = defaultZoom;
        this.setSliderValue();
    }

    /**
     * This method is called from within the constructor to initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is always
     * regenerated by the Form Editor.
     */
    @SuppressWarnings("unchecked")
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        Panel_CharacterCheckList = new javax.swing.JScrollPane();
        /*
        Make sure command line argument is the text file with the list of items.
        Open that file first and get list of items
        Then set up JTable with the list of items and a checkbox for each item

        */
        Table_CharacterChecklist = new javax.swing.JTable() {

            private static final long serialVersionUID = 1L;

            /*@Override
            public Class getColumnClass(int column) {
                return getValueAt(0, column).getClass();
            }*/

            @Override
            public boolean isCellEditable(int row, int column) {
                if(column==2) return true;
                return false;
            };

            @Override
            public Class getColumnClass(int column) {
                switch (column) {
                    case 0:
                    return String.class;
                    case 1:
                    return Integer.class;
                    default:
                    return Boolean.class;
                }
            }
        };
        Button_SaveSample = new javax.swing.JButton();
        Button_NextImageFile = new javax.swing.JButton();
        Button_PreviousImageFile = new javax.swing.JButton();
        Panel_MainImage =  new javax.swing.JPanel(){

            @Override
            public void paintComponent(Graphics g) {
                super.paintComponent(g);
                if(image!=null) {

                    if(this.getWidth() != mainPanelW || this.getHeight() != mainPanelH) {
                        scaledImage = getScaledImage(image);
                    }
                    if(scaledImage==null) { scaledImage = getScaledImage(image); }
                    if(scaledImage!=null) {
                        if(scaledImage.getWidth()!=scaledImageW || scaledImage.getHeight()!=scaledImageH) {
                            scaledImage = getScaledImage(image);
                        }
                    }
                    scaledImageW = scaledImage.getWidth();
                    scaledImageH = scaledImage.getHeight();
                    mainPanelW = this.getWidth();
                    mainPanelH = this.getHeight();

                    g.drawImage(scaledImage, 0, 0, this);

                    ArrayList<Point2D.Double> pts = sampledPoints.get(images[imageNumber]);

                    if (pts != null) {
                        int sN = pts.size();
                        g.setColor(sampledPointsColor);
                        for(Point2D.Double point : pts) {
                            int x = (int)((double)scaledImageW * point.x);
                            int y = (int)((double)scaledImageH * point.y);
                            g.drawRect(x, y, 1, 1);

                        }
                        Label_MainImage.setText("Main Image (Click to set zoomed image). Samples: " + sN);
                    }
                    //System.out.println("There are " + sN + " points already sampled from " + images[imageNumber]);

                    if(sampledXRandom >=0 && sampledYRandom >= 0 && CheckBoxMenuItem_RandomizeSampling.isSelected()) {

                        double zoom = checkZoom();
                        showW = (int) (((double) Panel_ZoomImage.getWidth() * (double) scaledImage.getWidth() / (double) image.getWidth()) / zoom);
                        showH = (int) (((double) Panel_ZoomImage.getHeight() * (double) scaledImage.getHeight() / (double) image.getHeight()) / zoom);

                        int wS = scaledImage.getWidth();
                        int hS = scaledImage.getHeight();

                        int w = image.getWidth();
                        int h = image.getHeight();

                        int sampledXRandomScaled = (int)((double)sampledXRandom * (double)wS/(double)w);
                        int sampledYRandomScaled = (int)((double)sampledYRandom * (double)hS/(double)h);

                        //int rectSize = (int)(0.0025 * (double)w);
                        //if(rectSize == 0 ) rectSize = 5;
                        //int rectX = sampledXRandomScaled - rectSize/2;
                        //int rectY = sampledYRandomScaled - rectSize/2;

                        int rectX = sampledXRandomScaled - showW/2;
                        int rectY = sampledYRandomScaled - showH/2;
                        if(rectX < 0) rectX =0;
                        if(rectY < 0) rectY = 0;
                        if(rectX+showW>=wS) rectX=wS-showW;
                        if(rectY+showH>=hS) rectY=hS-showH;
                        g.setColor(rectangleRandomColor);
                        g.drawRect(rectX, rectY, showW, showH);
                        g.setColor(pointRandomColor);
                        g.drawRect(sampledXRandomScaled, sampledYRandomScaled, 1, 1);
                    }
                    else if(mousePositionX >= 0 && mousePositionY >=0) {
                        g.setColor(rectangleSelectedColor);
                        setShowWidthHeight();
                        //int x = mousePositionX - showW/2;
                        //int y = mousePositionY - showH/2;
                        //if(x<0) x=0;
                        //if(y<0) y=0;
                        //if(x>this.getWidth()-showW) x = this.getWidth()-showW;
                        //if(y>this.getHeight()-showH) y = this.getHeight()-showH;
                        g.drawRect(mousePositionX, mousePositionY, showW, showH);
                        g.setColor(pointSelectedColor);
                        g.drawRect(selectedXMiddle, selectedYMiddle, 1, 1);
                    }
                }

                if (imageStateHash != null) {
                    setImageState(g);
                }
            }

        };
        Panel_ZoomImage = new javax.swing.JPanel() {
            @Override
            public void paintComponent(Graphics g) {
                super.paintComponent(g);
                //if(subImage!=null) {
                    //    scaledImage = getScaledImage(subImage);
                    //    g.drawImage(scaledImage, 0, 0, this);
                    //}
                //System.out.println("Repainting subimage");
                setSubimage();
                //System.out.println("Repainting subimage done");

                //g.setColor(Color.RED);
                //g.drawRect(selectedXZoom, selectedYZoom, 1, 1);
            }
        };
        Panel_SampledImage = new javax.swing.JPanel(){
            @Override
            public void paintComponent(Graphics g) {
                super.paintComponent(g);
                //if(currentSample!=null) {
                    //    scaledImage = getScaledImage(currentSample);
                    //    g.drawImage(scaledImage, 0, 0, this);
                    boolean createCurrentSample = true;
                    if(currentSample != null) createCurrentSample = false;
                    if(sampledXTrue >=0 && sampledYTrue >=0){
                        setSampleImage(sampledXTrue, sampledYTrue, createCurrentSample);
                    }
                }
            };
            Label_WindowSize = new javax.swing.JLabel();
            TextField_ZoomFactor = new javax.swing.JTextField();
            TextField_WindowSize = new javax.swing.JTextField();
            Button_RandomImage = new javax.swing.JButton();
            TextField_OverviewSize = new javax.swing.JTextField();
            Label_OverviewSize = new javax.swing.JLabel();
            ComboBox_AddedAttributeList = new javax.swing.JComboBox<>();
            Label_AddedAttribute = new javax.swing.JLabel();
            Label_LocalSize = new javax.swing.JLabel();
            TextField_LocalSize = new javax.swing.JTextField();
            Label_ClickMainImage = new javax.swing.JLabel();
            Label_ClickZoomedImage = new javax.swing.JLabel();
            Label_MainImage = new javax.swing.JLabel();
            Button_RandomPixel = new javax.swing.JButton();
            Button_ResetCharacters = new javax.swing.JButton();
            ComboBox_ImageFiles = new javax.swing.JComboBox<>();
            Slider_Zoom = new javax.swing.JSlider();
            TextField_ImageNumber = new javax.swing.JTextField();
            Label_SampleNumber = new javax.swing.JLabel();
            CheckBox_ResetOnSave = new javax.swing.JCheckBox();
            CheckBox_MutuallyExclusive = new javax.swing.JCheckBox();
            Menu_MainMenu = new javax.swing.JMenuBar();
            Menu_File = new javax.swing.JMenu();
            MenuItem_OpenCharactersFile = new javax.swing.JMenuItem();
            MenuItem_OpenImagesDirectory = new javax.swing.JMenuItem();
            MenuItem_OpenAddedAttributeFile = new javax.swing.JMenuItem();
            MenuItem_Exit = new javax.swing.JMenuItem();
            Menu_Parameters = new javax.swing.JMenu();
            MenuItem_SamplingParameters = new javax.swing.JMenuItem();
            MenuItem_ColorPalette = new javax.swing.JMenuItem();
            MenuItem_ImageInformation = new javax.swing.JMenuItem();
            Menu_Sampling = new javax.swing.JMenu();
            CheckBoxMenuItem_Autosave = new javax.swing.JCheckBoxMenuItem();
            CheckBoxMenuItem_RandomizeSampling = new javax.swing.JCheckBoxMenuItem();
            MenuItem_Delete = new javax.swing.JMenuItem();
            CheckBoxMenuItem_FastMode = new javax.swing.JCheckBoxMenuItem();
            Menu_Log = new javax.swing.JMenu();
            MenuItem_Notes = new javax.swing.JMenuItem();
            Menu_Help = new javax.swing.JMenu();
            jMenuItem1 = new javax.swing.JMenuItem();

            setDefaultCloseOperation(javax.swing.WindowConstants.EXIT_ON_CLOSE);

            int nChar = 0;
            Object[][] table = new Object[nChar][3];
            String[] title = {"Character", "N", "Selected"};
            Table_CharacterChecklist.setModel(new javax.swing.table.DefaultTableModel(
                table,
                title
            )
            /*{
                public void setValueAt(Object value, int row, int col) {

                    System.out.println("Hello from setValueAt");
                    super.setValueAt(value, row, col);

                    if(CheckBox_MutuallyExclusive.isSelected()) {

                        System.out.println("Mutually exclusive at row " + row);

                        int numChar = Table_CharacterChecklist.getModel().getRowCount();
                        for (int i = 0; i <= numChar - 1; i++) {
                            if(i != row) {
                                Table_CharacterChecklist.getModel().setValueAt(false,i, 2);

                            }
                        }
                    }
                    //           if (((Boolean) this.Table_CharacterChecklist.getModel().getValueAt(i, 2)))
                }

            }*/);
            Table_CharacterChecklist.addMouseListener(new java.awt.event.MouseAdapter() {
                public void mouseClicked(java.awt.event.MouseEvent evt) {
                    Table_CharacterChecklistMouseClicked(evt);
                }
                public void mousePressed(java.awt.event.MouseEvent evt) {
                    Table_CharacterChecklistMousePressed(evt);
                }
                public void mouseReleased(java.awt.event.MouseEvent evt) {
                    Table_CharacterChecklistMouseReleased(evt);
                }
            });
            Table_CharacterChecklist.addPropertyChangeListener(new java.beans.PropertyChangeListener() {
                public void propertyChange(java.beans.PropertyChangeEvent evt) {
                    Table_CharacterChecklistPropertyChange(evt);
                }
            });
            Panel_CharacterCheckList.setViewportView(Table_CharacterChecklist);

            Button_SaveSample.setText("Save Selection");
            Button_SaveSample.setEnabled(false);
            Button_SaveSample.setEnabled(false);
            Button_SaveSample.addMouseListener(new java.awt.event.MouseAdapter() {
                public void mouseClicked(java.awt.event.MouseEvent evt) {
                    Button_SaveSampleMouseClicked(evt);
                }
            });
            Button_SaveSample.addActionListener(new java.awt.event.ActionListener() {
                public void actionPerformed(java.awt.event.ActionEvent evt) {
                    Button_SaveSampleActionPerformed(evt);
                }
            });

            Button_NextImageFile.setText("Next File");
            Button_NextImageFile.setEnabled(false);
            Button_NextImageFile.addMouseListener(new java.awt.event.MouseAdapter() {
                public void mouseClicked(java.awt.event.MouseEvent evt) {
                    Button_NextImageFileMouseClicked(evt);
                }
            });
            Button_NextImageFile.addActionListener(new java.awt.event.ActionListener() {
                public void actionPerformed(java.awt.event.ActionEvent evt) {
                    Button_NextImageFileActionPerformed(evt);
                }
            });

            Button_PreviousImageFile.setText("Previous File");
            Button_PreviousImageFile.setEnabled(false);
            Button_PreviousImageFile.addMouseListener(new java.awt.event.MouseAdapter() {
                public void mouseClicked(java.awt.event.MouseEvent evt) {
                    Button_PreviousImageFileMouseClicked(evt);
                }
            });

            Panel_MainImage.setBorder(javax.swing.BorderFactory.createLineBorder(new java.awt.Color(0, 0, 0)));
            Panel_MainImage.setCursor(new java.awt.Cursor(java.awt.Cursor.CROSSHAIR_CURSOR));
            Panel_MainImage.addMouseMotionListener(new java.awt.event.MouseMotionAdapter() {
                public void mouseMoved(java.awt.event.MouseEvent evt) {
                    Panel_MainImageMouseMoved(evt);
                }
            });
            Panel_MainImage.addMouseListener(new java.awt.event.MouseAdapter() {
                public void mouseEntered(java.awt.event.MouseEvent evt) {
                    Panel_MainImageMouseEntered(evt);
                }
                public void mousePressed(java.awt.event.MouseEvent evt) {
                    Panel_MainImageMousePressed(evt);
                }
            });

            javax.swing.GroupLayout Panel_MainImageLayout = new javax.swing.GroupLayout(Panel_MainImage);
            Panel_MainImage.setLayout(Panel_MainImageLayout);
            Panel_MainImageLayout.setHorizontalGroup(
                Panel_MainImageLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                .addGap(0, 0, Short.MAX_VALUE)
            );
            Panel_MainImageLayout.setVerticalGroup(
                Panel_MainImageLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                .addGap(0, 0, Short.MAX_VALUE)
            );

            Panel_ZoomImage.setBorder(javax.swing.BorderFactory.createLineBorder(new java.awt.Color(0, 0, 0)));
            Panel_ZoomImage.setCursor(new java.awt.Cursor(java.awt.Cursor.CROSSHAIR_CURSOR));
            Panel_ZoomImage.addMouseMotionListener(new java.awt.event.MouseMotionAdapter() {
                public void mouseDragged(java.awt.event.MouseEvent evt) {
                    Panel_ZoomImageMouseDragged(evt);
                }
            });
            Panel_ZoomImage.addMouseWheelListener(new java.awt.event.MouseWheelListener() {
                public void mouseWheelMoved(java.awt.event.MouseWheelEvent evt) {
                    Panel_ZoomImageMouseWheelMoved(evt);
                }
            });
            Panel_ZoomImage.addMouseListener(new java.awt.event.MouseAdapter() {
                public void mousePressed(java.awt.event.MouseEvent evt) {
                    Panel_ZoomImageMousePressed(evt);
                }
            });
            Panel_ZoomImage.addComponentListener(new java.awt.event.ComponentAdapter() {
                public void componentResized(java.awt.event.ComponentEvent evt) {
                    Panel_ZoomImageComponentResized(evt);
                }
            });

            javax.swing.GroupLayout Panel_ZoomImageLayout = new javax.swing.GroupLayout(Panel_ZoomImage);
            Panel_ZoomImage.setLayout(Panel_ZoomImageLayout);
            Panel_ZoomImageLayout.setHorizontalGroup(
                Panel_ZoomImageLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                .addGap(0, 0, Short.MAX_VALUE)
            );
            Panel_ZoomImageLayout.setVerticalGroup(
                Panel_ZoomImageLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                .addGap(0, 215, Short.MAX_VALUE)
            );

            Panel_SampledImage.setBorder(javax.swing.BorderFactory.createLineBorder(new java.awt.Color(0, 0, 0)));
            Panel_SampledImage.addComponentListener(new java.awt.event.ComponentAdapter() {
                public void componentResized(java.awt.event.ComponentEvent evt) {
                    Panel_SampledImageComponentResized(evt);
                }
            });

            javax.swing.GroupLayout Panel_SampledImageLayout = new javax.swing.GroupLayout(Panel_SampledImage);
            Panel_SampledImage.setLayout(Panel_SampledImageLayout);
            Panel_SampledImageLayout.setHorizontalGroup(
                Panel_SampledImageLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                .addGap(0, 0, Short.MAX_VALUE)
            );
            Panel_SampledImageLayout.setVerticalGroup(
                Panel_SampledImageLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                .addGap(0, 215, Short.MAX_VALUE)
            );

            Label_WindowSize.setText("Medium");

            TextField_ZoomFactor.setColumns(3);
            TextField_ZoomFactor.setText("1000");
            TextField_ZoomFactor.addActionListener(new java.awt.event.ActionListener() {
                public void actionPerformed(java.awt.event.ActionEvent evt) {
                    TextField_ZoomFactorActionPerformed(evt);
                }
            });
            TextField_ZoomFactor.addKeyListener(new java.awt.event.KeyAdapter() {
                public void keyPressed(java.awt.event.KeyEvent evt) {
                    TextField_ZoomFactorKeyPressed(evt);
                }
            });

            TextField_WindowSize.setColumns(3);
            TextField_WindowSize.setText("1000");
            TextField_WindowSize.addKeyListener(new java.awt.event.KeyAdapter() {
                public void keyPressed(java.awt.event.KeyEvent evt) {
                    TextField_WindowSizeKeyPressed(evt);
                }
            });

            Button_RandomImage.setText("Random Image");
            Button_RandomImage.setEnabled(false);
            Button_RandomImage.addMouseListener(new java.awt.event.MouseAdapter() {
                public void mouseClicked(java.awt.event.MouseEvent evt) {
                    Button_RandomImageMouseClicked(evt);
                }
            });

            TextField_OverviewSize.setColumns(3);
            TextField_OverviewSize.setText("20000");
            TextField_OverviewSize.addKeyListener(new java.awt.event.KeyAdapter() {
                public void keyPressed(java.awt.event.KeyEvent evt) {
                    TextField_OverviewSizeKeyPressed(evt);
                }
            });

            Label_OverviewSize.setText("Large");

            ComboBox_AddedAttributeList.setModel(new javax.swing.DefaultComboBoxModel<>(new String[] {  }));
            ComboBox_AddedAttributeList.addActionListener(new java.awt.event.ActionListener() {
                public void actionPerformed(java.awt.event.ActionEvent evt) {
                    ComboBox_AddedAttributeListActionPerformed(evt);
                }
            });

            Label_AddedAttribute.setText("Added Attribute");

            Label_LocalSize.setText("Small");

            TextField_LocalSize.setColumns(3);
            TextField_LocalSize.setText("1000");
            TextField_LocalSize.addKeyListener(new java.awt.event.KeyAdapter() {
                public void keyPressed(java.awt.event.KeyEvent evt) {
                    TextField_LocalSizeKeyPressed(evt);
                }
            });

            Label_ClickMainImage.setText("Zoom:");

            Label_ClickZoomedImage.setText("Selected Sample (Click above to set):");

            Label_MainImage.setText("Main Image (Click to set zoomed imaged):");

            Button_RandomPixel.setText("Next Random Pixel");
            Button_RandomPixel.setEnabled(false);
            Button_RandomPixel.addMouseListener(new java.awt.event.MouseAdapter() {
                public void mouseClicked(java.awt.event.MouseEvent evt) {
                    Button_RandomPixelMouseClicked(evt);
                }
            });

            Button_ResetCharacters.setText("Reset Characters");
            Button_ResetCharacters.setEnabled(false);
            Button_ResetCharacters.addMouseListener(new java.awt.event.MouseAdapter() {
                public void mouseClicked(java.awt.event.MouseEvent evt) {
                    Button_ResetCharactersMouseClicked(evt);
                }
            });

            ComboBox_ImageFiles.addItemListener(new java.awt.event.ItemListener() {
                public void itemStateChanged(java.awt.event.ItemEvent evt) {
                    ComboBox_ImageFilesItemStateChanged(evt);
                }
            });

            Slider_Zoom.setMaximum(1500);
            Slider_Zoom.setMinimum(20);
            Slider_Zoom.setValue((int)(this.defaultZoom*100.0));
            Slider_Zoom.addChangeListener(new javax.swing.event.ChangeListener() {
                public void stateChanged(javax.swing.event.ChangeEvent evt) {
                    Slider_ZoomStateChanged(evt);
                }
            });

            TextField_ImageNumber.setColumns(5);
            TextField_ImageNumber.addActionListener(new java.awt.event.ActionListener() {
                public void actionPerformed(java.awt.event.ActionEvent evt) {
                    TextField_ImageNumberActionPerformed(evt);
                }
            });
            TextField_ImageNumber.addKeyListener(new java.awt.event.KeyAdapter() {
                public void keyPressed(java.awt.event.KeyEvent evt) {
                    TextField_ImageNumberKeyPressed(evt);
                }
            });

            Label_SampleNumber.setText("Sample Number: 0");

            CheckBox_ResetOnSave.setSelected(true);
            CheckBox_ResetOnSave.setText("Reset on save");

            CheckBox_MutuallyExclusive.setSelected(true);
            CheckBox_MutuallyExclusive.setText("Mutually Exclusive Characters");

            Menu_File.setText("File");

            MenuItem_OpenCharactersFile.setText("Select Character File...");
            MenuItem_OpenCharactersFile.addMenuKeyListener(new javax.swing.event.MenuKeyListener() {
                public void menuKeyPressed(javax.swing.event.MenuKeyEvent evt) {
                    MenuItem_OpenCharactersFileMenuKeyPressed(evt);
                }
                public void menuKeyReleased(javax.swing.event.MenuKeyEvent evt) {
                }
                public void menuKeyTyped(javax.swing.event.MenuKeyEvent evt) {
                }
            });
            MenuItem_OpenCharactersFile.addActionListener(new java.awt.event.ActionListener() {
                public void actionPerformed(java.awt.event.ActionEvent evt) {
                    MenuItem_OpenCharactersFileActionPerformed(evt);
                }
            });
            Menu_File.add(MenuItem_OpenCharactersFile);

            MenuItem_OpenImagesDirectory.setText("Select Images Directory...");
            MenuItem_OpenImagesDirectory.addActionListener(new java.awt.event.ActionListener() {
                public void actionPerformed(java.awt.event.ActionEvent evt) {
                    MenuItem_OpenImagesDirectoryActionPerformed(evt);
                }
            });
            Menu_File.add(MenuItem_OpenImagesDirectory);

            MenuItem_OpenAddedAttributeFile.setText("Select Added Attribute File...");
            MenuItem_OpenAddedAttributeFile.addActionListener(new java.awt.event.ActionListener() {
                public void actionPerformed(java.awt.event.ActionEvent evt) {
                    MenuItem_OpenAddedAttributeFileActionPerformed(evt);
                }
            });
            Menu_File.add(MenuItem_OpenAddedAttributeFile);

            MenuItem_Exit.setText("Exit");
            MenuItem_Exit.addMenuKeyListener(new javax.swing.event.MenuKeyListener() {
                public void menuKeyPressed(javax.swing.event.MenuKeyEvent evt) {
                }
                public void menuKeyReleased(javax.swing.event.MenuKeyEvent evt) {
                    MenuItem_ExitMenuKeyReleased(evt);
                }
                public void menuKeyTyped(javax.swing.event.MenuKeyEvent evt) {
                }
            });
            MenuItem_Exit.addActionListener(new java.awt.event.ActionListener() {
                public void actionPerformed(java.awt.event.ActionEvent evt) {
                    MenuItem_ExitActionPerformed(evt);
                }
            });
            Menu_File.add(MenuItem_Exit);

            Menu_MainMenu.add(Menu_File);

            Menu_Parameters.setText("Parameters");

            MenuItem_SamplingParameters.setText("Set Sampling Parameters...");
            MenuItem_SamplingParameters.addActionListener(new java.awt.event.ActionListener() {
                public void actionPerformed(java.awt.event.ActionEvent evt) {
                    MenuItem_SamplingParametersActionPerformed(evt);
                }
            });
            Menu_Parameters.add(MenuItem_SamplingParameters);

            MenuItem_ColorPalette.setText("Set Color Palette...");
            MenuItem_ColorPalette.addActionListener(new java.awt.event.ActionListener() {
                public void actionPerformed(java.awt.event.ActionEvent evt) {
                    MenuItem_ColorPaletteActionPerformed(evt);
                }
            });
            Menu_Parameters.add(MenuItem_ColorPalette);

            MenuItem_ImageInformation.setText("View Image Information...");
            MenuItem_ImageInformation.setEnabled(false);
            MenuItem_ImageInformation.addActionListener(new java.awt.event.ActionListener() {
                public void actionPerformed(java.awt.event.ActionEvent evt) {
                    MenuItem_ImageInformationActionPerformed(evt);
                }
            });
            Menu_Parameters.add(MenuItem_ImageInformation);

            Menu_MainMenu.add(Menu_Parameters);

            Menu_Sampling.setText("Sampling");

            CheckBoxMenuItem_Autosave.setText("Autosave");
            CheckBoxMenuItem_Autosave.setEnabled(false);
            CheckBoxMenuItem_Autosave.addItemListener(new java.awt.event.ItemListener() {
                public void itemStateChanged(java.awt.event.ItemEvent evt) {
                    CheckBoxMenuItem_AutosaveItemStateChanged(evt);
                }
            });
            Menu_Sampling.add(CheckBoxMenuItem_Autosave);

            CheckBoxMenuItem_RandomizeSampling.setText("Randomize Sampling");
            CheckBoxMenuItem_RandomizeSampling.setEnabled(false);
            CheckBoxMenuItem_RandomizeSampling.addItemListener(new java.awt.event.ItemListener() {
                public void itemStateChanged(java.awt.event.ItemEvent evt) {
                    CheckBoxMenuItem_RandomizeSamplingItemStateChanged(evt);
                }
            });
            CheckBoxMenuItem_RandomizeSampling.addActionListener(new java.awt.event.ActionListener() {
                public void actionPerformed(java.awt.event.ActionEvent evt) {
                    CheckBoxMenuItem_RandomizeSamplingActionPerformed(evt);
                }
            });
            Menu_Sampling.add(CheckBoxMenuItem_RandomizeSampling);

            MenuItem_Delete.setText("Delete Samples...");
            MenuItem_Delete.addActionListener(new java.awt.event.ActionListener() {
                public void actionPerformed(java.awt.event.ActionEvent evt) {
                    MenuItem_DeleteActionPerformed(evt);
                }
            });
            Menu_Sampling.add(MenuItem_Delete);

            CheckBoxMenuItem_FastMode.setSelected(true);
            CheckBoxMenuItem_FastMode.setText("Fast Mode");
            CheckBoxMenuItem_FastMode.addItemListener(new java.awt.event.ItemListener() {
                public void itemStateChanged(java.awt.event.ItemEvent evt) {
                    CheckBoxMenuItem_FastModeItemStateChanged(evt);
                }
            });
            Menu_Sampling.add(CheckBoxMenuItem_FastMode);

            Menu_MainMenu.add(Menu_Sampling);

            Menu_Log.setText("Log");

            MenuItem_Notes.setText("Edit Notes...");
            MenuItem_Notes.addActionListener(new java.awt.event.ActionListener() {
                public void actionPerformed(java.awt.event.ActionEvent evt) {
                    MenuItem_NotesActionPerformed(evt);
                }
            });
            Menu_Log.add(MenuItem_Notes);

            Menu_MainMenu.add(Menu_Log);

            Menu_Help.setText("Help");

            jMenuItem1.setText("Help...");
            Menu_Help.add(jMenuItem1);

            Menu_MainMenu.add(Menu_Help);

            setJMenuBar(Menu_MainMenu);

            javax.swing.GroupLayout layout = new javax.swing.GroupLayout(getContentPane());
            getContentPane().setLayout(layout);
            layout.setHorizontalGroup(
                layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                .addGroup(layout.createSequentialGroup()
                    .addContainerGap()
                    .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                        .addGroup(layout.createSequentialGroup()
                            .addComponent(Button_PreviousImageFile)
                            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                            .addComponent(Button_NextImageFile)
                            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                            .addComponent(Button_RandomImage)
                            .addGap(0, 0, Short.MAX_VALUE))
                        .addComponent(ComboBox_ImageFiles, javax.swing.GroupLayout.Alignment.TRAILING, 0, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                        .addGroup(layout.createSequentialGroup()
                            .addComponent(Label_MainImage, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                            .addGap(12, 12, 12)
                            .addComponent(TextField_ImageNumber, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                        .addComponent(Panel_MainImage, javax.swing.GroupLayout.Alignment.TRAILING, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                    .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                    .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                        .addGroup(layout.createSequentialGroup()
                            .addGap(6, 6, 6)
                            .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING, false)
                                .addGroup(layout.createSequentialGroup()
                                    .addComponent(Label_ClickMainImage)
                                    .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                    .addComponent(Slider_Zoom, javax.swing.GroupLayout.PREFERRED_SIZE, 115, javax.swing.GroupLayout.PREFERRED_SIZE)
                                    .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                    .addComponent(TextField_ZoomFactor, javax.swing.GroupLayout.PREFERRED_SIZE, 50, javax.swing.GroupLayout.PREFERRED_SIZE))
                                .addGroup(javax.swing.GroupLayout.Alignment.LEADING, layout.createSequentialGroup()
                                    .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                                        .addGroup(layout.createSequentialGroup()
                                            .addComponent(TextField_LocalSize, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                                            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED))
                                        .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, layout.createSequentialGroup()
                                            .addComponent(Label_LocalSize)
                                            .addGap(25, 25, 25)))
                                    .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                                        .addGroup(layout.createSequentialGroup()
                                            .addComponent(Label_WindowSize)
                                            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                                            .addComponent(Label_OverviewSize))
                                        .addGroup(layout.createSequentialGroup()
                                            .addComponent(TextField_WindowSize, javax.swing.GroupLayout.PREFERRED_SIZE, 49, javax.swing.GroupLayout.PREFERRED_SIZE)
                                            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                            .addComponent(TextField_OverviewSize, javax.swing.GroupLayout.PREFERRED_SIZE, 49, javax.swing.GroupLayout.PREFERRED_SIZE)))
                                    .addGap(0, 0, Short.MAX_VALUE)))
                            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                        .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, layout.createSequentialGroup()
                            .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
                                .addComponent(Panel_ZoomImage, javax.swing.GroupLayout.Alignment.LEADING, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                                .addComponent(Panel_SampledImage, javax.swing.GroupLayout.Alignment.LEADING, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                                .addGroup(javax.swing.GroupLayout.Alignment.LEADING, layout.createSequentialGroup()
                                    .addGap(6, 6, 6)
                                    .addComponent(Label_ClickZoomedImage)
                                    .addGap(0, 0, Short.MAX_VALUE)))
                            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)))
                    .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                        .addComponent(Panel_CharacterCheckList, javax.swing.GroupLayout.Alignment.TRAILING, javax.swing.GroupLayout.PREFERRED_SIZE, 0, Short.MAX_VALUE)
                        .addGroup(layout.createSequentialGroup()
                            .addComponent(Label_AddedAttribute)
                            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                            .addComponent(ComboBox_AddedAttributeList, 0, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                        .addGroup(layout.createSequentialGroup()
                            .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                                .addComponent(CheckBox_MutuallyExclusive)
                                .addGroup(layout.createSequentialGroup()
                                    .addComponent(Label_SampleNumber)
                                    .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                                    .addComponent(Button_RandomPixel)))
                            .addGap(0, 0, Short.MAX_VALUE))
                        .addGroup(layout.createSequentialGroup()
                            .addComponent(CheckBox_ResetOnSave)
                            .addGap(25, 25, 25)
                            .addComponent(Button_ResetCharacters)
                            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                            .addComponent(Button_SaveSample))))
            );
            layout.setVerticalGroup(
                layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                .addGroup(layout.createSequentialGroup()
                    .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                        .addComponent(Label_LocalSize)
                        .addComponent(Label_AddedAttribute)
                        .addComponent(ComboBox_AddedAttributeList, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addComponent(Button_PreviousImageFile)
                        .addComponent(Button_NextImageFile)
                        .addComponent(Button_RandomImage)
                        .addComponent(Label_WindowSize)
                        .addComponent(Label_OverviewSize))
                    .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                    .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                        .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                            .addComponent(ComboBox_ImageFiles, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                            .addComponent(TextField_LocalSize, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                        .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                            .addComponent(TextField_OverviewSize, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                            .addComponent(Button_RandomPixel)
                            .addComponent(TextField_WindowSize, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                            .addComponent(Label_SampleNumber)))
                    .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                    .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                        .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                            .addComponent(Label_ClickMainImage)
                            .addComponent(Label_MainImage)
                            .addComponent(Slider_Zoom, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                            .addComponent(TextField_ZoomFactor, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                            .addComponent(TextField_ImageNumber, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                        .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                            .addComponent(CheckBox_ResetOnSave)
                            .addComponent(Button_ResetCharacters)
                            .addComponent(Button_SaveSample)))
                    .addGap(9, 9, 9)
                    .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                        .addGroup(layout.createSequentialGroup()
                            .addComponent(Panel_ZoomImage, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                            .addComponent(Label_ClickZoomedImage)
                            .addGap(12, 12, 12)
                            .addComponent(Panel_SampledImage, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                        .addComponent(Panel_MainImage, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                        .addGroup(layout.createSequentialGroup()
                            .addComponent(Panel_CharacterCheckList)
                            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                            .addComponent(CheckBox_MutuallyExclusive)))
                    .addContainerGap())
            );

            pack();
        }// </editor-fold>//GEN-END:initComponents

    private void Panel_MainImageMouseMoved(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_Panel_MainImageMouseMoved
        //mousePositionX = evt.getX();
        //mousePositionY = evt.getY();

        this.setSelectedScaled(evt.getX(), evt.getY(), this.MOUSE_MOVED);

        if (image != null && this.scaledImage != null && trackMouse) {

            int zoomPanelW = Panel_ZoomImage.getWidth();
            int zoomPanelH = Panel_ZoomImage.getHeight();

            //int mainPanelW = this.Panel_MainImage.getWidth();
            //int mainPanelH = this.Panel_MainImage.getHeight();
            int scaledW = this.scaledImage.getWidth();
            int scaledH = this.scaledImage.getHeight();

            int trueW = image.getWidth();
            int trueH = image.getHeight();

            double zoom = checkZoom();
            this.currentZoom = zoom;

            int imagePositionX = (int) ((double) (mousePositionX * trueW) / (double) scaledW);
            int imagePositionY = (int) ((double) (mousePositionY * trueH) / (double) scaledH);
            //x = selectedXTrue;
            //y = selectedYTrue;
            //jPanel2.repaint();

            this.setShowWidthHeight();

            //showW = (int) (((double) zoomPanelW * (double) scaledW / (double) trueW) / zoom);
            //showH = (int) (((double) zoomPanelH * (double) scaledH / (double) trueH) / zoom);
            try {
                //Graphics g = Panel_MainImage.getGraphics();
                Graphics g1 = this.Panel_ZoomImage.getGraphics();

                //subImage = image.getSubimage(x, y, (int) ((double) zoomPanelW / zoom), (int) ((double) zoomPanelH / zoom));
                subImage = getSubimage(image, imagePositionX, imagePositionY, (int) ((double) zoomPanelW / zoom), (int) ((double) zoomPanelH / zoom));
                //SCALE_REPLICATE
                subImage = scaleImage(subImage, zoomPanelW, zoomPanelH, BufferedImage.TYPE_INT_RGB, Image.SCALE_SMOOTH);
                g1.drawImage(subImage, 0, 0, Panel_ZoomImage);
                this.Panel_MainImage.repaint();
                //g.drawRect(mousePositionX, mousePositionY, showW, showH);

            } catch (Exception e) {
            }

        }


    }//GEN-LAST:event_Panel_MainImageMouseMoved

    public void setLabel_MainImageText(String text) {
        this.Label_MainImage.setText(text);
    }

    private void enableRandomAndAutosaveSampling() {

        this.CheckBoxMenuItem_Autosave.setEnabled(true);
        this.CheckBoxMenuItem_RandomizeSampling.setEnabled(true);

    }

    private void disableRandomAndAutosaveSampling() {

        this.CheckBoxMenuItem_Autosave.setSelected(false);
        this.CheckBoxMenuItem_Autosave.setEnabled(false);
        this.CheckBoxMenuItem_RandomizeSampling.setEnabled(false);
        this.CheckBoxMenuItem_RandomizeSampling.setSelected(false);
        sampledXRandom = -1;
        sampledYRandom = -1;
    }

    private void checkSaveButton() {
        //System.out.println("CheckSave");
        if (this.CheckBoxMenuItem_FastMode.isSelected()) {
            this.Button_SaveSample.setEnabled(true);
            disableRandomAndAutosaveSampling();
            return;
        }
        if (this.CheckBoxMenuItem_Autosave.isSelected()) {
            this.Button_SaveSample.setEnabled(true);
            //disableRandomAndAutosaveSampling();
        } else {
            if (this.imagesEnabled && this.attributesEnabled && this.charactersEnabled) {
                if (currentSample != null) {
                    this.Button_SaveSample.setEnabled(true);
                } else {
                    this.Button_SaveSample.setEnabled(false);
                }
                this.CheckBoxMenuItem_Autosave.setEnabled(true);
                this.CheckBoxMenuItem_RandomizeSampling.setEnabled(true);
            } else {
                this.Button_SaveSample.setEnabled(false);
                //disableRandomAndAutosaveSampling();
            }
        }
        //this.repaint();
    }

    private void printImageInfo(String imgFile) {
        File f = new File(imgFile);
        try {
            image = ImageIO.read(f);
            System.out.println("Aspect ratio (w/h) " + ((double) image.getWidth() / (double) image.getHeight()) + " Width " + image.getWidth() + " File " + imgFile);

        } catch (IOException ex) {
            Logger.getLogger(Frame_DataSampler.class
                    .getName()).log(Level.SEVERE, null, ex);
        }

        //this.TextField_ImageNumber.setText(Integer.toString(imageNumber));
    }

    public void setImagesDirectory(String directory, String rootDir, boolean newProject) {

        String dir;
        imageNumber++;
        //set the directory
        //if (!absolute) {
        //    this.defaultImagesDirectory = directory;

        this.rootDirectory = rootDir;
        //dir = this.rootDirectory + separator + directory;
        // } else {
        //    System.out.println("Absolute");
        this.defaultImagesDirectory = directory;
        dir = rootDir + separator + directory;

        //get list of image files
        try {
            //File f = new File(rootDir + separator + defaultImagesDirectory);
            File f = new File(dir);
            images = f.list();
            if (images == null) {
                JOptionPane.showMessageDialog(this, "No images found in provided " + dir + " directory. Be sure your root directory path is valid.");
                return;
            } else if (images.length <= 0) {
                JOptionPane.showMessageDialog(this, "No images found in provided directory. Place all images you wish to analyze in the " + dir + " directory.");
                return;
            }
            this.imageFilesHash = new Hashtable<String, Integer>(images.length);
            for (int i = 0; i < images.length; i++) {
                this.imageFilesHash.put(images[i], i);
                //     printImageInfo(rootDir + separator + defaultImagesDirectory + separator + images[i]);

            }
            //this.Label_ImagesDirectory.setText("Current directory: " + dirName);
            if (images.length > 0) {
                //imageNumber = 0;
                if (newProject) {
                    imageNumber = ThreadLocalRandom.current().nextInt(0, images.length);
                }

                //System.out.println("The selected starting image is " + imageNumber + " " + images[imageNumber]);
                this.manualSelection = false;
                setFileComboBox(imageNumber);
                setImageNumberTextField();
                this.manualSelection = true;
                //System.out.println("Calling from setImagesDirectory");
                setMainImage(dir + separator + images[imageNumber]);
                this.imagesEnabled = true;

                this.CheckBoxMenuItem_RandomizeSampling.setSelected(false);

                checkSaveButton();
            }
        } catch (Exception e) {
            e.printStackTrace();
            Dialog_FileNotFound d_fnf = new Dialog_FileNotFound(this, true, defaultImagesDirectory);
            d_fnf.setVisible(true);
            defaultImagesDirectory = "Images";
            this.resetVariables(true, "setImagesDirectory exeption");
            //image = null;
            //this.subImage = null;
            //this.scaledImage = null;
            //this.imagesEnabled = false;
            //this.CheckBoxMenuItem_RandomizeSampling.setEnabled(false);
            checkSaveButton();
        }
        //set main image

    }

    private void setFileComboBox(int selected) {
        this.ComboBox_ImageFiles.removeAllItems();
        for (String image1 : images) {
            this.ComboBox_ImageFiles.addItem(image1);
        }

        this.setFileComboBox(images[selected]);
    }

    private void setMainImage(int x, int y) {

        if (image == null) {
            this.resetVariables(false, "setMainImage image is null");
            return;
        }

        //BufferedImage orig = deepCopy(image);
        //image.getGraphics().drawRect(x,y, 50,50);
        //image.setRGB(x, y, Color.ORANGE.getRGB());
        Panel_MainImage.repaint();

        //image = orig;
    }

    private void setMainImage(int imgNum) {
        setMainImage(this.rootDirectory + separator + defaultImagesDirectory + separator + images[imgNum]);
    }

    public void setMainImage(String imgFile) {

        try {
            System.out.println("Attempting to set " + imgFile);

            /*
            if (this.dii == null) {
                dii = new Dialog_ImageInformation(this, false, imgFile);
            }
            dii.setImagePath(imgFile);
             */
            this.MenuItem_ImageInformation.setEnabled(true);
            File f = new File(imgFile);
            image = ImageIO.read(f);

            if (image == null) {
                JOptionPane.showMessageDialog(this, "It looks like " + imgFile + " is empty. Going to next image.");
                this.imageNumber++;
                if (imageNumber >= images.length) {
                    imageNumber = 0;
                }
                setMainImage(imageNumber);
                return;
            }

            if (image.getWidth() / image.getHeight() > 1) {
                JOptionPane.showMessageDialog(this, "It looks like " + imgFile + " is not an herbarium specimen. Image height should be larger than width. Going to next image.");
                this.imageNumber++;
                if (imageNumber >= images.length) {
                    imageNumber = 0;
                }
                setMainImage(imageNumber);
                return;
            }

            if (image.getWidth() < this.defaultImageWidth) {
                JOptionPane.showMessageDialog(this, "It looks like " + imgFile + "'s size is too small. Image width should be at least " + this.defaultImageWidth + " pixels. Going to next image.");
                this.imageNumber++;
                if (imageNumber >= images.length) {
                    imageNumber = 0;
                }
                setMainImage(imageNumber);
                return;
            }

            int newWidth = this.defaultImageWidth;

            //account for colorbar on the side, which increases the width to height ratio. Empirically, when colorbar was on the size, the ratio was >0.7 and took up about 1/10 of the width of the image
            if ((double) image.getWidth() / (double) image.getHeight() > 0.7) {
                double add = (double) defaultImageWidth / 9.0;
                newWidth = defaultImageWidth + (int) add;
            }

            imageScaling = (double) newWidth / (double) image.getWidth();

            int newHeight = (int) ((double) image.getHeight() * imageScaling);
            //System.out.println("The original dimensions were " + image.getWidth() + " by " + image.getHeight() + " and the new dimensions are " + newWidth + " by " + newHeight + " with image scaling " + this.imageScaling);
            image = scaleImage(image, newWidth, newHeight, BufferedImage.TYPE_INT_RGB, Image.SCALE_SMOOTH);

            //System.out.println("The aspect ratio is (w/h)" + ((double) image.getWidth() / (double) image.getHeight()) + " and the width is " + image.getWidth());
            //this.TextField_ImageNumber.setText(Integer.toString(imageNumber));
            this.resetVariables(true, "setMainImage");

            this.resetZoomPanels();
            this.setImageFileButtons();

            if (this.CheckBoxMenuItem_RandomizeSampling.isSelected()) {

                //JOptionPane.showMessageDialog(this, "<html>Push OK to classify pixels for randomization. <br>Please be patient. You will be alterted when complete...</html>");
                classifyPixels(false);
                //JOptionPane.showMessageDialog(this, "Finished classifying pixels");

            }
            //System.out.println("Done");
            this.imageFile = imgFile;
            this.setAttributeFromFilename();

        } catch (IOException e) {
            Dialog_FileNotFound d_fnf = new Dialog_FileNotFound(this, true, imgFile);
            d_fnf.setVisible(true);
            this.MenuItem_ImageInformation.setEnabled(false);
            this.imageScaling = -1.0;
            this.resetVariables(true, "setMainImage exeption");
            image = null;
            //this.scaledImage = null;
            //this.subImage = null;
            //this.zoomImage = null;
        }

        //Panel_MainImagePanel.repaint();
        this.setSampleNumber();
        this.repaint();

    }

    private void setImageState(Graphics g) {
        if (imageStateHash != null) {
            String state = this.imageStateHash.get(images[imageNumber]);
            if (state == null) {
                return;
            }

            g.setColor(new Color(255, 0, 0));
            g.drawString(state, 20, 20);
        } else {
            System.out.println("Could not set image state due to null imageStateHash");
        }
    }

    private void resetVariables(boolean deleteClassification, String from) {
        //System.out.println("Resetting variables from " + from);
        this.scaledImage = null;
        this.subImage = null;
        this.zoomImage = null;
        this.currentSample = null;

        selectedXScaled = -1;
        selectedYScaled = -1;
        selectedXTrue = -1;
        selectedYTrue = -1;
        sampledXTrue = -1;
        sampledYTrue = -1;
        sampledXRandom = -1;
        sampledYRandom = -1;

        mousePositionX = -1;
        mousePositionY = -1;

        scaledImageW = -1;
        scaledImageH = -1;

        mainPanelW = -1;
        mainPanelH = -1;

        if (deleteClassification) {
            classification = null;
        }
    }

    private BufferedImage getSelectedPixelImage(int x, int y, boolean setOverviewSize) {
        //BufferedImage pi = image.getSubimage(x, y, 1, 1);
        BufferedImage pi = getSubimage(image, x, y, 1, 1);

        if (setOverviewSize) {
            //int localSize = this.checkLocalSize();
            //int windowSize = this.checkWindowSize();
            int overviewSize = this.checkOverviewSize();

            pi = this.scaleImage(pi, overviewSize, overviewSize, BufferedImage.TYPE_INT_RGB, Image.SCALE_REPLICATE);

            pi = this.setPositionInformation(pi);
            pi = this.setAttributeInformation(pi);
        }
        return (pi);
    }

    private BufferedImage setPositionInformation(BufferedImage img) {
        int size = img.getHeight();
        int sizeP = size / 3;

        int posX = 255 * sampledXTrue / image.getWidth();
        int posY = 255 * sampledYTrue / image.getHeight();

        BufferedImage tempX = new BufferedImage(1, 1, img.getType());

        Color t;
        try {
            t = new Color(posX, posX, posX);
        } catch (Exception e) {
            return (img);
        }
        tempX.setRGB(0, 0, t.getRGB());
        tempX = this.scaleImage(tempX, sizeP, sizeP, img.getType(), Image.SCALE_REPLICATE);

        BufferedImage tempY = new BufferedImage(1, 1, img.getType());
        t = new Color(posY, posY, posY);
        tempY.setRGB(0, 0, t.getRGB());
        tempY = this.scaleImage(tempY, sizeP, sizeP, img.getType(), Image.SCALE_REPLICATE);

        Graphics g = img.getGraphics();
        g.drawImage(tempX, 0, 0, null);
        g.drawImage(tempY, 0, sizeP, null);

        return (img);
    }

    private BufferedImage setAttributeInformation(BufferedImage img) {

        if (this.ComboBox_AddedAttributeList == null) {
            return img;
        }
        if (this.ComboBox_AddedAttributeList.getSelectedItem() == null) {
            return img;
        }
        if (this.attributesHash == null) {
            return img;
        }
        if (this.attributesHash.size() <= 0) {
            return img;
        }

        int size = img.getHeight();
        int sizeP = size / 3;

        String att = (String) this.ComboBox_AddedAttributeList.getSelectedItem();
        String tVal = (String) this.attributesHash.get(att);
        int val = Integer.parseInt(tVal);
        int sizeH = this.attributesHash.size();

        //System.out.println(att);
        //System.out.println("'" + (String) this.attributesHash.get(att) + "'");
        //System.out.println(this.attributesHash.size());
        if (att != null) {

            if (val > sizeH) {
                JOptionPane.showMessageDialog(this, "It is likely that your attribute file has duplicate rows in it or is poorly formated. Quitting.");
                System.exit(0);
            }

            int posX = (int) (255.0 * (double) val / (double) sizeH);

            //System.out.println("The value of the attribute pixel is " + posX + " sizeH is " + sizeH + " and val is " + val);
            BufferedImage tempX = new BufferedImage(1, 1, img.getType());
            Color t = new Color(posX, posX, posX);
            tempX.setRGB(0, 0, t.getRGB());
            tempX = this.scaleImage(tempX, sizeP, sizeP, img.getType(), Image.SCALE_REPLICATE);

            Graphics g = img.getGraphics();
            g.drawImage(tempX, 0, 2 * sizeP, null);
        }

        return (img);
    }

    private BufferedImage getWindowImage(int x, int y, boolean setOverviewSize) {
        int winSize = this.checkWindowSize();

        int overviewSize = this.checkOverviewSize();
        if (!setOverviewSize) {
            overviewSize = winSize;
        }

        return createSampleSubimage(x, y, winSize, overviewSize, overviewSize, BufferedImage.TYPE_INT_RGB, Image.SCALE_REPLICATE);
    }

    private int checkLocalSize() {
        int winSize = checkWindowSize();
        int localSize = Integer.parseInt(this.TextField_LocalSize.getText());
        if (localSize % 2 == 0) {
            localSize++; //make windowsize odd
        }
        if (localSize < 3 || localSize > image.getHeight() || localSize > image.getWidth() || localSize > winSize) {
            localSize = defaultLocalSize;
        }
        this.TextField_LocalSize.setText(String.valueOf(localSize));
        return localSize;
    }

    private int checkWindowSize() {
        int winSize = Integer.parseInt(this.TextField_WindowSize.getText());
        if (winSize < 0 || winSize > image.getHeight() || winSize > image.getWidth()) {
            winSize = defaultWindowSize;
        }
        if (winSize % 2 == 0) {
            winSize++; //make windowsize odd
        }
        this.TextField_WindowSize.setText(String.valueOf(winSize));
        return winSize;
    }

    private int checkOverviewSize() {
        int winSize = checkWindowSize();
        int ovSize = Integer.parseInt(this.TextField_OverviewSize.getText());
        if (ovSize % 2 == 0) {
            ovSize++; //make windowsize odd
        }
        if (ovSize < 0 || ovSize > image.getHeight() || ovSize > image.getWidth() || ovSize < winSize) {
            ovSize = defaultOverviewSize;
        }
        this.TextField_OverviewSize.setText(String.valueOf(ovSize));
        return ovSize;
    }

    private double checkZoom() {
        double zoom = Double.parseDouble(this.TextField_ZoomFactor.getText());
        if (zoom < 0) {
            zoom = defaultZoom;
        }
        this.TextField_ZoomFactor.setText(String.valueOf(zoom));
        currentZoom = zoom;
        return zoom;
    }

    private BufferedImage getZoomOutImage(int x, int y, boolean useOverviewSize) {

        int ovSize = checkOverviewSize();
        //int winSize = this.finalOverviewSize;
        int winSize = ovSize;
        if (!useOverviewSize) {
            winSize = checkWindowSize();
        }

        return createSampleSubimage(x, y, ovSize, winSize, winSize, BufferedImage.TYPE_INT_RGB, Image.SCALE_SMOOTH);
    }

    public BufferedImage scaleImage(BufferedImage orig, int width, int height, int imgType, int scaleType) {
        //System.out.println("Scaling image to " + width + " width.");
        Image imgData = orig.getScaledInstance(width, height, scaleType);
        orig = new BufferedImage(imgData.getWidth(null), imgData.getHeight(null), imgType);
        orig.getGraphics().drawImage(imgData, 0, 0, null);
        return (orig);
    }

    private BufferedImage createSampleSubimage(int x, int y, int size, int endSizeW, int endSizeH, int imgType, int scaleType) {
        int offset = (size - 1) / 2;
        BufferedImage tempImg = new BufferedImage(size, size, imgType);
        for (int i = -offset; i <= offset; i++) {
            int truex = x + i;
            for (int j = -offset; j <= offset; j++) {
                int truey = y + j;
                if (truex < 0 || truex > image.getWidth() - 1 || truey < 0 || truey > image.getHeight() - 1) {
                    Color black = new Color(0, 0, 0);
                    tempImg.setRGB(i + offset, j + offset, black.getRGB());
                } else {
                    tempImg.setRGB(i + offset, j + offset, image.getRGB(truex, truey));
                }
            }
        }
        //int winSize = checkWindowSize();
        tempImg = scaleImage(tempImg, endSizeW, endSizeH, imgType, scaleType);
        return tempImg;
    }

    private BufferedImage getLocalImage(int x, int y, boolean scale) {
        int locSize = checkLocalSize();
        int overviewSize = checkOverviewSize();

        if (!scale) {
            overviewSize = locSize;
        }

        return createSampleSubimage(x, y, locSize, overviewSize, overviewSize, BufferedImage.TYPE_INT_RGB, Image.SCALE_REPLICATE);
    }

    private BufferedImage createSampleImage(int x, int y) {

        //int windowSize = this.checkWindowSize();
        //int localSize = this.checkLocalSize();
        int overviewSize = this.checkOverviewSize();

        BufferedImage pi = getSelectedPixelImage(x, y, true);
        BufferedImage wi = getWindowImage(x, y, true);
        BufferedImage li = getLocalImage(x, y, true);
        BufferedImage zoi = getZoomOutImage(x, y, true);

        //zoi = scaleImage(zoi, windowSize, windowSize, BufferedImage.TYPE_INT_RGB, Image.SCALE_SMOOTH);
        BufferedImage si = new BufferedImage(2 * overviewSize, 2 * overviewSize, BufferedImage.TYPE_INT_RGB);
        Graphics g = si.getGraphics();
        g.drawImage(pi, 0, 0, null);
        g.drawImage(li, overviewSize, 0, null);
        g.drawImage(wi, 0, overviewSize, null);
        g.drawImage(zoi, overviewSize, overviewSize, null);
        return si;
    }

    //random x, random y
    public void setRandomPixelSubimage(int x, int y) {
        if (image != null) {

            int sizeX = Panel_ZoomImage.getWidth();
            int sizeY = Panel_ZoomImage.getHeight();
            int w = image.getWidth();
            int h = image.getHeight();

            double zoom = checkZoom();
            int subWidth = (int) ((double) sizeX / zoom);
            int subHeight = (int) ((double) sizeY / zoom);

            int boxX = x - subWidth / 2;
            int boxY = y - subHeight / 2;

            int newX = subWidth / 2;
            int newY = subHeight / 2;

            //this.sampledXRandom = newX;
            //this.sampledYRandom = newY;
            if (boxX < 0) {
                int diff = 0 - boxX;
                boxX = 0;
                newX = boxX - diff;
            } else if (boxX + subWidth > w) {
                int diff = (boxX + subWidth) - w;
                boxX = boxX - diff;
                newX = newX + diff;
            }

            if (boxY < 0) {
                int diff = 0 - boxY;
                boxY = 0;
                newY = boxY - diff;
            } else if (boxY + subHeight > h) {
                int diff = (boxY + subHeight) - h;
                boxY = boxY - diff;
                newY = newY + diff;
            }

            try {
                Graphics g = Panel_ZoomImage.getGraphics();

                //subImage = image.getSubimage(boxX, boxY, subWidth, subHeight);
                subImage = getSubimage(image, boxX, boxY, subWidth, subHeight);
                BufferedImage subImageOrig = subImage;
                subImage.setRGB(newX, newY, this.pointZoomColor.getRGB());
                //SCALE_REPLICATE
                subImage = scaleImage(subImage, sizeX, sizeY, BufferedImage.TYPE_INT_RGB, Image.SCALE_SMOOTH);
                g.drawImage(subImage, 0, 0, Panel_ZoomImage);
                subImage = subImageOrig;
            } catch (Exception e) {
            }

        }
    }

    public static BufferedImage getSubimage(BufferedImage image, int x, int y, int w, int h) {
        BufferedImage newImage = new BufferedImage(w, h,
                BufferedImage.TYPE_INT_RGB);
        Graphics2D g = newImage.createGraphics();
        g.drawRenderedImage(image,
                AffineTransform.getTranslateInstance(-x, -y));
        g.dispose();
        return newImage;
    }

    public static BufferedImage deepCopy(BufferedImage bi) {
        ColorModel cm = bi.getColorModel();
        boolean isAlphaPremultiplied = cm.isAlphaPremultiplied();
        WritableRaster raster = bi.copyData(null);
        return new BufferedImage(cm, raster, isAlphaPremultiplied, null);
    }

    public void setSubimage() {
        if (image != null && selectedXScaled >= 0 && selectedYScaled >= 0) {
            int x = selectedXScaled;
            int y = selectedYScaled;
            int sizeX = Panel_ZoomImage.getWidth();
            int sizeY = Panel_ZoomImage.getHeight();
            int w = image.getWidth();
            int h = image.getHeight();

            selectedXTrue = (int) ((double) (x * image.getWidth()) / (double) scaledImage.getWidth());
            selectedYTrue = (int) ((double) (y * image.getHeight()) / (double) scaledImage.getHeight());

            x = selectedXTrue;
            y = selectedYTrue;

            //jPanel2.repaint();
            try {
                Graphics g = Panel_ZoomImage.getGraphics();
                double zoom = checkZoom();
                //this.currentZoom = zoom;
                //subImage = image.getSubimage(x, y, (int) ((double) sizeX / zoom), (int) ((double) sizeY / zoom));
                subImage = getSubimage(image, x, y, (int) ((double) sizeX / zoom), (int) ((double) sizeY / zoom));
                //SCALE_REPLICATE
                subImage = scaleImage(subImage, sizeX, sizeY, BufferedImage.TYPE_INT_RGB, Image.SCALE_SMOOTH);
                g.drawImage(subImage, 0, 0, Panel_ZoomImage);

            } catch (Exception e) {
            }

        }
    }

    private void updateCharacterList() {
        if (characters == null) {
            return;
        }
        int nChar = characters.size();
        Object[][] table = new Object[nChar][3];
        for (int i = 0; i < nChar; i++) {
            table[i][0] = characters.get(i);
            table[i][1] = 0;
            table[i][2] = false;
        }

        String[] title = {"Character", "N", "Selected"};
        Table_CharacterChecklist.setModel(new javax.swing.table.DefaultTableModel(
                table,
                title
        ));

    }

    public void setInitialAttributesList() {
        String attsFile = this.rootDirectory + separator + this.defaultAttributesDirectory + separator + defaultAttributesFile;
        try {
            if (Files.exists(Paths.get(rootDirectory + separator + defaultSamplesDirectory + separator + projectName + separator + defaultAttributesFile))) {
                attsFile = rootDirectory + separator + defaultSamplesDirectory + separator + projectName + separator + defaultAttributesFile;
            }
        } catch (Exception e) {

        }
        setAttributesList(attsFile);

    }

    public void setAttributesList(String attsFile) {

        this.attributesFile = attsFile;
        this.attributesHash = new Hashtable<String, String>(500);

        try {
            this.ComboBox_AddedAttributeList.removeAllItems();
            BufferedReader br = new BufferedReader(new FileReader(attsFile));
            String line;
            while ((line = br.readLine()) != null) {
                String[] attData = line.split("\\t");

                if (attData.length == 2) {

                    attributesHash.put(attData[1], attData[0]);
                    this.ComboBox_AddedAttributeList.addItem(attData[1]);
                }

            }
            br.close();

            System.out.println("There are " + this.attributesHash.size() + " attributes.");

            this.attributesEnabled = true;
            this.setAttributeFromFilename();
            checkSaveButton();

        } catch (Exception e) {
            //e.printStackTrace();
            Dialog_FileNotFound d_fnf = new Dialog_FileNotFound(this, true, attsFile);
            d_fnf.setVisible(true);
            this.attributesFile = null;
            this.attributesEnabled = false;
            checkSaveButton();
        }

    }

    private void setAttributeFromFilename() {
        if (this.imageFile != null && this.attributesEnabled) {
            String fileName = new File(imageFile).getName();
            String attribute = parseAttributeFromFileName(fileName, "_");
            this.currentSpecies = attribute;
            ComboBox_AddedAttributeList.setSelectedItem(attribute);
            //System.out.println("Setting attribute to " + attribute);
        }
    }

    private String parseAttributeFromFileName(String info, String regExp) {
        String[] split = info.split(regExp);
        String attribute = null;
        if (split.length >= 3) {
            attribute = split[0] + "_" + split[1];
        }
        return attribute;
    }

    public void setInitialCharacterList() {
        String charFile = this.rootDirectory + separator + this.defaultCharactersDirectory + separator + defaultCharactersFile;
        try {
            if (Files.exists(Paths.get(rootDirectory + separator + defaultSamplesDirectory + separator + projectName + separator + defaultCharactersFile))) {
                charFile = rootDirectory + separator + defaultSamplesDirectory + separator + projectName + separator + defaultCharactersFile;
            }
        } catch (Exception e) {

        }
        setCharacterList(charFile);
    }

    public void setCharacterList(String charFile) {
        this.characterFile = charFile;
        characters = new ArrayList<String>();
        charactersHash = new Hashtable<String, Boolean>();

        try {
            BufferedReader br = new BufferedReader(new FileReader(charFile));
            String line;
            while ((line = br.readLine()) != null) {
                characters.add(line);
            }
            br.close();
            updateCharacterList();
            setCharacterHash();
            this.charactersEnabled = true;
            this.Button_ResetCharacters.setEnabled(true);
            checkSaveButton();

            System.out.println("There are " + this.charactersHash.size() + " characters.");

        } catch (Exception e) {
            // e.printStackTrace();
            Dialog_FileNotFound d_fnf = new Dialog_FileNotFound(this, true, charFile);
            d_fnf.setVisible(true);
            this.characterFile = null;
            this.charactersEnabled = false;
            checkSaveButton();
        }

    }

    private void setCharacterHash() {
        if (this.characters == null) {
            return;
        }
        if (this.characters.size() == 0) {
            return;
        }
        for (String character : characters) {
            String[] splitCharacters = character.split(characterSplitString, 0);
            if (splitCharacters.length > 0) {
                for (String splitCharacter : splitCharacters) {
                    charactersHash.put(splitCharacter, false);
                }
            }
        }
    }

    private void resetTableCharacters() {
        int numChar = this.Table_CharacterChecklist.getModel().getRowCount();
        for (int i = 0; i <= numChar - 1; i++) {
            this.Table_CharacterChecklist.setValueAt(false, i, 2);
        }
    }

    public static Dimension getScaledDimension(Dimension imgSize, Dimension boundary) {

        int original_width = imgSize.width;
        int original_height = imgSize.height;
        int bound_width = boundary.width;
        int bound_height = boundary.height;
        int new_width = original_width;
        int new_height = original_height;

        // first check if we need to scale width
        if (original_width > bound_width) {
            //scale width to fit
            new_width = bound_width;
            //scale height to maintain aspect ratio
            new_height = (new_width * original_height) / original_width;
        }

        // then check if we need to scale even with the new height
        if (new_height > bound_height) {
            //scale height to fit instead
            new_height = bound_height;
            //scale width to maintain aspect ratio
            new_width = (new_height * original_width) / original_height;
        }

        return new Dimension(new_width, new_height);
    }

    private BufferedImage getScaledImage(BufferedImage image) {
        int pw = Panel_MainImage.getWidth();
        int ph = Panel_MainImage.getHeight();
        //double panelAspectRatio = (double) pw / (double) ph;

        int iw = image.getWidth();
        int ih = image.getHeight();
        //double imageAspectRatio = (double) iw / (double) ih;

        Dimension scaled = getScaledDimension(new Dimension(iw, ih), new Dimension(pw, ph));
        int width = scaled.width;
        int height = scaled.height;
        //double width;
        //double height;

        //THIS IS NOT CORRECT!!
        //if (panelAspectRatio < imageAspectRatio) {
        //width = (double) pw;
        //height = (double) pw / imageAspectRatio;
        //} else {
        //height = (double) ph;
        //width = (double) pw * imageAspectRatio;
        //}
        //System.out.println("Original image width was " + iw);
        scaledImage = this.scaleImage(image, width, height, BufferedImage.TYPE_INT_RGB, Image.SCALE_SMOOTH);

        return (scaledImage);
    }

    private void setShowWidthHeight() {

        if (image == null || scaledImage == null) {
            showW = -1;
            showH = -1;
            return;
        }
        int zoomPanelW = Panel_ZoomImage.getWidth();
        int zoomPanelH = Panel_ZoomImage.getHeight();

        //int mainPanelW = this.Panel_MainImage.getWidth();
        //int mainPanelH = this.Panel_MainImage.getHeight();
        int scaledW = this.scaledImage.getWidth();
        int scaledH = this.scaledImage.getHeight();

        int trueW = image.getWidth();
        int trueH = image.getHeight();

        double zoom = checkZoom();
        this.currentZoom = zoom;

        showW = (int) (((double) zoomPanelW * (double) scaledW / (double) trueW) / zoom);
        showH = (int) (((double) zoomPanelH * (double) scaledH / (double) trueH) / zoom);

    }

    private void setSelectedScaled(int x, int y, int type) {

        if (this.scaledImage == null) {
            return;
        }

        setShowWidthHeight();

        x = x - showW / 2;
        y = y - showH / 2;

        if (x < 0) {
            x = 0;
        }
        if (y < 0) {
            y = 0;
        }

        /*
        if (x > this.Panel_MainImage.getWidth() - showW) {
            x = this.Panel_MainImage.getWidth() - showW;
        }
        if (y > this.Panel_MainImage.getHeight() - showH) {
            y = this.Panel_MainImage.getHeight() - showH;
        }
         */
        if (x > this.scaledImage.getWidth() - showW) {
            x = this.scaledImage.getWidth() - showW - 1;
        }
        if (y > this.scaledImage.getHeight() - showH) {
            y = this.scaledImage.getHeight() - showH - 1;
        }

        if (type == this.MOUSE_MOVED) {
            this.mousePositionX = x;
            this.mousePositionY = y;
        } else if (type == this.MOUSE_PRESSED || type == this.SLIDER_CHANGED) {
            selectedXScaled = x;
            selectedYScaled = y;
        }
    }

    private void Panel_MainImageMousePressed(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_Panel_MainImageMousePressed
        trackMouse = false;
        int x = evt.getX();
        int y = evt.getY();
        selectedXMiddle = x;
        selectedYMiddle = y;
        setSelectedScaled(x, y, this.MOUSE_PRESSED);

//        if (this.scaledImage.getWidth() - selectedX < this.Panel_ZoomImage.getWidth()) {
//            selectedX = this.Panel_ZoomImage.getWidth();
//        }
//        if (this.scaledImage.getHeight() - selectedY < this.Panel_ZoomImage.getHeight()) {
//            selectedY = this.scaledImage.getHeight() - this.Panel_ZoomImage.getHeight();
//        }
        setSubimage();
        this.Panel_MainImage.repaint();
    }//GEN-LAST:event_Panel_MainImageMousePressed

    private void Panel_ZoomImageMousePressed(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_Panel_ZoomImageMousePressed

        //System.out.println("pressed");

        if (this.CheckBoxMenuItem_RandomizeSampling.isSelected() && this.CheckBoxMenuItem_RandomizeSampling.isEnabled()) {
            JOptionPane.showMessageDialog(null, "<html>User sampling of pixels is disabled when random sampling is enabled. <br>Disable random sampling under the Sampling menu to sample pixels, or click 'Next Random Pixel'.</html>");
            return;
        }
        if (subImage == null || this.scaledImage == null) {
            this.Panel_SampledImage.setBackground(Color.white);
            return;
        }

        this.selectedXZoom = evt.getX();
        this.selectedYZoom = evt.getY();

        int x = (int) ((double) selectedXTrue + (double) selectedXZoom / (double) currentZoom);
        int y = (int) ((double) selectedYTrue + (double) selectedYZoom / (double) currentZoom);

        sampledXTrue = x;
        sampledYTrue = y;

        setSampleImage(sampledXTrue, sampledYTrue, true);
        checkSaveButton();
        //setSampleImage(sampledXTrue, sampledYTrue, true);

        //if (this.CheckBox_Autosave.isSelected()) {
        if (this.CheckBoxMenuItem_Autosave.isSelected() && this.CheckBoxMenuItem_Autosave.isEnabled()) {
            saveSample();
        }
        try {
            // System.out.println("pressed try to draw point");
            //this.Panel_ZoomImage.repaint();
            Graphics g1 = this.Panel_ZoomImage.getGraphics();
            //     System.out.println("pressed try to draw point a");
            g1.setColor(this.pointZoomColor);
            //    System.out.println("pressed try to draw point b");
            g1.drawRect(selectedXZoom, selectedYZoom, 1, 1);
            //   System.out.println("pressed try to draw point point is drawn c");

        } catch (Exception e) {
            //         System.out.println("exception in pressed");
        }
    }//GEN-LAST:event_Panel_ZoomImageMousePressed

    private void setSampleImage(int x, int y, boolean createCurrentSample) {
        //System.out.println("Setting sample image");
        if (createCurrentSample) {
            currentSample = this.createSampleImage(x, y);
        }

        int sizeX = this.Panel_SampledImage.getWidth();
        int sizeY = this.Panel_SampledImage.getHeight();
        Graphics g = this.Panel_SampledImage.getGraphics();
        BufferedImage scaledSample = scaleImage(currentSample, sizeX, sizeY, BufferedImage.TYPE_INT_RGB, Image.SCALE_SMOOTH);
        g.drawImage(scaledSample, 0, 0, this.Panel_SampledImage);

    }

    private void MenuItem_ExitActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_MenuItem_ExitActionPerformed
        close();
    }//GEN-LAST:event_MenuItem_ExitActionPerformed

    private void MenuItem_ExitMenuKeyReleased(javax.swing.event.MenuKeyEvent evt) {//GEN-FIRST:event_MenuItem_ExitMenuKeyReleased
        close();
    }//GEN-LAST:event_MenuItem_ExitMenuKeyReleased

    private void close() {
        if (this.partitionByFile) {
            if (this.filePartitionType != null) {
                if (this.filePartitionType.size() > 0) {
                    readAndCreateSamplePartitionMapping(true);
                }
            }
        }
        this.closeSessionNotes();
        System.exit(0);
    }

    private void MenuItem_OpenCharactersFileMenuKeyPressed(javax.swing.event.MenuKeyEvent evt) {//GEN-FIRST:event_MenuItem_OpenCharactersFileMenuKeyPressed

    }//GEN-LAST:event_MenuItem_OpenCharactersFileMenuKeyPressed

    private void MenuItem_OpenCharactersFileActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_MenuItem_OpenCharactersFileActionPerformed
        if (scfb == null) {
            scfb = new Frame_SelectCharactersFileBrowser(this);
        }
        scfb.setVisible(true);
        scfb.setDirectory();
    }//GEN-LAST:event_MenuItem_OpenCharactersFileActionPerformed

    private void MenuItem_OpenImagesDirectoryActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_MenuItem_OpenImagesDirectoryActionPerformed
        if (sid == null) {
            sid = new Frame_SelectImagesDirectory(this);
        }
        sid.setVisible(true);
        sid.setDirectory();
    }//GEN-LAST:event_MenuItem_OpenImagesDirectoryActionPerformed

    private void TextField_ZoomFactorActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_TextField_ZoomFactorActionPerformed
        this.mousePositionX = -1;
        this.mousePositionY = -1;
        if (this.CheckBoxMenuItem_RandomizeSampling.isSelected()) {
            this.randomlySelectPixel();
        } else {
            setSubimage();
        }

        setSliderValue();
    }//GEN-LAST:event_TextField_ZoomFactorActionPerformed

    private void setFileComboBox(String item) {
        this.manualSelection = false;
        this.ComboBox_ImageFiles.setSelectedItem(item);
        this.manualSelection = true;
    }

    private void setImageNumberTextField() {
        this.TextField_ImageNumber.setText(Integer.toString(imageNumber));
    }

    private void Button_PreviousImageFileMouseClicked(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_Button_PreviousImageFileMouseClicked

        setPreviousImage();
    }//GEN-LAST:event_Button_PreviousImageFileMouseClicked

    private void setPreviousImage() {
        imageNumber--;
        if (imageNumber < 0) {
            imageNumber = images.length - 1;
        }
        setFileComboBox(images[imageNumber]);
        setImageNumberTextField();

        //setFileComboBox(imageNumber);
        //System.out.println("Calling from previous button");
        setMainImage(this.rootDirectory + separator + defaultImagesDirectory + separator + images[imageNumber]);
    }

    private void Button_NextImageFileActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_Button_NextImageFileActionPerformed
        // TODO add your handling code here:
    }//GEN-LAST:event_Button_NextImageFileActionPerformed

    private void Button_NextImageFileMouseClicked(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_Button_NextImageFileMouseClicked
        setNextImage();
    }//GEN-LAST:event_Button_NextImageFileMouseClicked

    private void setNextImage() {
        imageNumber++;
        if (imageNumber > images.length - 1) {
            imageNumber = 0;
        }

        setFileComboBox(images[imageNumber]);
        setImageNumberTextField();
        //setFileComboBox(imageNumber);
        //System.out.println("Calling from next button");
        setMainImage(this.rootDirectory + separator + defaultImagesDirectory + separator + images[imageNumber]);

    }

    private void Button_RandomImageMouseClicked(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_Button_RandomImageMouseClicked
        //this.Button_RandomImage.setText("loading...");
        //repaint();
        imageNumber = ThreadLocalRandom.current().nextInt(0, images.length); //note that the max end is not inclusive, so no -1
        setFileComboBox(images[imageNumber]);
        setImageNumberTextField();
        //setFileComboBox(imageNumber);
        //System.out.println("Calling from random button");
        setMainImage(this.rootDirectory + separator + defaultImagesDirectory + separator + images[imageNumber]);
        //this.Button_RandomImage.setText("Random Image");
        //repaint();
    }//GEN-LAST:event_Button_RandomImageMouseClicked

    private void ComboBox_AddedAttributeListActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_ComboBox_AddedAttributeListActionPerformed
        // TODO add your handling code here:
    }//GEN-LAST:event_ComboBox_AddedAttributeListActionPerformed

    private void MenuItem_OpenAddedAttributeFileActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_MenuItem_OpenAddedAttributeFileActionPerformed
        if (saafb == null) {
            saafb = new Frame_SelectAdditionalAttributeFileBrowser(this);
        }
        saafb.setVisible(true);
        saafb.setDirectory();
    }//GEN-LAST:event_MenuItem_OpenAddedAttributeFileActionPerformed

    private String createHeader() {

        if (charactersHash == null) {
            JOptionPane.showMessageDialog(this, "Illegally reached 'createHeader' when the characters are null");
            System.exit(0);
        }
        if (charactersHash.size() <= 0) {
            JOptionPane.showMessageDialog(this, "Illegally reached 'createHeader' with no characters");
            System.exit(0);
        }
        String charHeader = new String();
        headerHash = new Hashtable<Integer, String>();
        //int numChar = this.Table_CharacterChecklist.getModel().getRowCount();
        //for (int i = 0; i <= numChar - 1; i++) {
        int cnt = 0;
        //make the characters ordered in the data sheet
        TreeMap charTreeMap = new TreeMap<String, Boolean>(charactersHash);
        Set<String> keys = charTreeMap.keySet();
        for (String character : keys) {
            //for(int i=0; i< this.charactersHash.size(); i++) {
            //this.Table_CharacterChecklist.getModel().getValueAt(i, 0);
            //charHeader = charHeader + this.Table_CharacterChecklist.getModel().getValueAt(i, 0) + "\t";

            headerHash.put(cnt, character);
            cnt++;

            if (cnt == 1) {
                charHeader = character;
            } else {
                charHeader = charHeader + "\t" + character;
            }
        }
        String header = "Sample Number\tUser\tProject Name\tTimestamp\tX Original\tY Original\tX Scaled\tY Scaled\tScaled Image Width\tSmall Window\tMedium Window\tLarge Window\t" + charHeader + "\tAttribute Number\tSpecies\tImage File\n";
        return header;
    }

    private BufferedImage scaleMontage() {
        //scale the current sample to the output montage size
        BufferedImage copy = getSubimage(this.currentSample, 0, 0, currentSample.getWidth(), currentSample.getHeight());
        return this.scaleImage(copy, this.savedMontageSize, this.savedMontageSize, BufferedImage.TYPE_INT_RGB, Image.SCALE_SMOOTH);

    }

    private BufferedImage scaleOverviewImage(BufferedImage overview) {
        BufferedImage copy = getSubimage(overview, 0, 0, overview.getWidth(), overview.getHeight());
        return this.scaleImage(copy, this.savedOverviewSize, this.savedOverviewSize, BufferedImage.TYPE_INT_RGB, Image.SCALE_SMOOTH);
    }

    private void setSampleNumber() {
        this.readSampledPixels(true);
        this.Label_SampleNumber.setText("Project " + this.projectName + " sample size: " + this.sampleNumber);
        setLabel_MainImageText("Main Image (Click to set zoomed image) samples: 0");
        if (this.sampledPoints != null) {
            if (this.images != null) {
                if (this.imageNumber >= 0) {
                    if (this.sampledPoints.get(images[imageNumber]) != null) {
                        int curN = this.sampledPoints.get(images[imageNumber]).size();
                        setLabel_MainImageText("Main Image (Click to set zoomed image) samples: " + curN);
                    }
                }
            }
        }
    }

    public void checkDirectories() {

        if (!Files.exists(Paths.get(this.rootDirectory))) {
            try {
                Files.createDirectories(Paths.get(this.rootDirectory));
            } catch (Exception ex) {
                JOptionPane.showMessageDialog(this, "Unable to detect root directory: " + this.rootDirectory + ". Please be sure the provided root directory is accessible from your machine. Quitting.");
                //this.rootDirectory = System.getProperty("user.dir") + separator + "DNNSampling";
                System.exit(0);
            }
        }

        try {
            Files.createDirectories(Paths.get(rootDirectory + separator + defaultImagesDirectory));
        } catch (IOException ex) {

            //Logger.getLogger(Frame_Parameters.class.getName()).log(Level.SEVERE, null, ex);
            JOptionPane.showMessageDialog(this, "Could not create Images directory. Be sure your path information is valid. Quitting.");
            System.exit(0);
        }

        try {
            Files.createDirectories(Paths.get(rootDirectory + separator + defaultCharactersDirectory));
        } catch (IOException ex) {
            //Logger.getLogger(Frame_Parameters.class.getName()).log(Level.SEVERE, null, ex);
            JOptionPane.showMessageDialog(this, "Could not create Characters directory. Be sure your path information is valid. Quitting.");
            System.exit(0);

        }

        try {
            Files.createDirectories(Paths.get(rootDirectory + separator + defaultAttributesDirectory));
        } catch (IOException ex) {
            JOptionPane.showMessageDialog(this, "Could not create Attributes directory. Be sure your path information is valid. Quitting.");
            System.exit(0);

            //Logger.getLogger(Frame_Parameters.class.getName()).log(Level.SEVERE, null, ex);
        }

        try {
            Files.createDirectories(Paths.get(rootDirectory + separator + defaultSamplesDirectory));
        } catch (IOException ex) {
            JOptionPane.showMessageDialog(this, "Could not create Samples directory. Be sure your path information is valid. Quitting.");
            System.exit(0);

        }

        try {
            Files.createDirectories(Paths.get(rootDirectory + separator + defaultSamplesDirectory + separator + projectName));
        } catch (IOException ex) {
            JOptionPane.showMessageDialog(this, "Could not create Project Samples directory. Be sure your path information is valid. Quitting.");
            System.exit(0);
        }

        for (String type : types) {
            try {
                Files.createDirectories(Paths.get(rootDirectory + separator + defaultSamplesDirectory + separator + this.projectName + separator + type));
                for (String imageType : imageTypes) {
                    Files.createDirectories(Paths.get(rootDirectory + separator + defaultSamplesDirectory + separator + this.projectName + separator + type + separator + imageType));
                }
                //Files.createDirectories(Paths.get(rootDirectory + separator + defaultSamplesDirectory + separator + this.projectName + separator + type + separator + "Pixel"));
                //Files.createDirectories(Paths.get(rootDirectory + separator + defaultSamplesDirectory + separator + this.projectName + separator + type + separator + "Local"));
                //Files.createDirectories(Paths.get(rootDirectory + separator + defaultSamplesDirectory + separator + this.projectName + separator + type + separator + "Window"));
                //Files.createDirectories(Paths.get(rootDirectory + separator + defaultSamplesDirectory + separator + this.projectName + separator + type + separator + "Overview"));
                //Files.createDirectories(Paths.get(rootDirectory + separator + defaultSamplesDirectory + separator + this.projectName + separator + type + separator + "Montage"));
            } catch (IOException ex) {
                JOptionPane.showMessageDialog(this, "Could not create Project Samples directory for " + type + ". Be sure your path information is valid. Quitting.");
                System.exit(0);
            }
        }

//Logger.getLogger(Frame_Parameters.class.getName()).log(Level.SEVERE, null, ex);
    }

    private String setSaveDirectory() {

        this.checkDirectories();
        String type;
        if (this.CheckBoxMenuItem_FastMode.isSelected()) {
            return rootDirectory + separator + defaultSamplesDirectory + separator + projectName;
        } else if (this.partitionByFile) {
            this.readAndCreateSamplePartitionMapping(true);
            type = this.filePartitionType.get(images[imageNumber]);
        } else {
            double[] probs = {(double) defaultTrainingPercentage / 100.0, (double) defaultTestingPercentage / 100.0, (double) defaultValidationPercentage / 100};

            type = randPMF(probs, types);
        }
        return rootDirectory + separator + defaultSamplesDirectory + separator + projectName + separator + type;
    }

    private void resetCharacterHash() {
        if (charactersHash == null) {
            return;
        }
        if (charactersHash.size() <= 0) {
            return;
        }
        for (String character : charactersHash.keySet()) {
            charactersHash.put(character, false);
        }
    }

    private String createStateString() {
        resetCharacterHash();
        if (this.headerHash == null) {
            String header = createHeader();
        }
//extract character states
        boolean stateSelected = false;
        String state = new String();
        String imageState = new String();
        int numChar = this.Table_CharacterChecklist.getModel().getRowCount();
        for (int i = 0; i <= numChar - 1; i++) {
            if (((Boolean) this.Table_CharacterChecklist.getModel().getValueAt(i, 2))) {
                //state += "1\t";
                String character = (String) this.Table_CharacterChecklist.getModel().getValueAt(i, 0);
                String[] splitCharacters = character.split(characterSplitString, 0);
                if (splitCharacters != null) {
                    if (splitCharacters.length > 0) {
                        for (String splitCharacter : splitCharacters) {
                            this.charactersHash.put(splitCharacter, true);
                        }

                    } else {
                        JOptionPane.showMessageDialog(this, "Reached a non-existant character");
                        System.exit(0);
                    }
                }
                stateSelected = true;
                int samp = (Integer) this.Table_CharacterChecklist.getModel().getValueAt(i, 1) + 1;
                this.Table_CharacterChecklist.getModel().setValueAt(samp, i, 1);
            } else {
                //state += "0\t";
            }
        }

        for (int i = 0; i < headerHash.size(); i++) {
            if (this.charactersHash.get(headerHash.get(i))) {
                state += "1\t";
                imageState += " " + headerHash.get(i);
            } else {
                state += "0\t";
            }
        }
        if (!stateSelected) {
            return null;
        }

        if (imageStateHash != null) {
            imageStateHash.put(images[imageNumber], imageState);
        }
        return state;
    }

    private void onFirstSave() {

        this.disablePartitionTypeSelection = true;
        if (fp == null) {
            fp = new Frame_Parameters(this);
        }
        if (fsc == null) {
            fsc = new Frame_SelectionColors(this);
        }
        if (fnt == null) {
            fnt = new Frame_NoteTaker(this);
        }

        //set paramter fields to be uneditable for this project (should be done on first save)
        disableParameterEditing();
        //write project parameters to a file
        writeProjectParametersFile();
        //fp.setPartitionEnabling();
        if (this.partitionByFile) {
            this.writeSamplePartitionMappingToFile();
        }
    }

    private void saveFastMode() {

        onFirstSave();

        try {
            //to assure that the timestamp is unique for each sample, sleep for 5 milliseconds
            Thread.sleep(5);
        } catch (InterruptedException ex) {

        }
        long time = new Date().getTime();
        sampleNumber++;

        //String directory = this.rootDirectory + separator + defaultSamplesDirectory + separator + this.projectName;
        String directory = this.setSaveDirectory();

        //System.out.println(directory);
        //check if csv exists, and if not create it and write a header to it
        String sampleDataFile = directory + separator + dataFile;
        if (!Files.exists(Paths.get(sampleDataFile))) {
            try {
                BufferedWriter writer = new BufferedWriter(new FileWriter(sampleDataFile));
                String header = createHeader();
                writer.write(header);
                writer.close();

                JOptionPane.showMessageDialog(this, "Creating data file " + sampleDataFile);

            } catch (IOException ex) {
                Logger.getLogger(Frame_DataSampler.class
                        .getName()).log(Level.SEVERE, null, ex);
            }
        }

        boolean savedState = false;

        String state = createStateString(); //includes trailing tab

        if (state == null) {
            JOptionPane.showMessageDialog(this, "<html>No characters were selected. At least one character must be selected. Not saving...<br>If none of the categories match, create a category for 'Other'.</html>");
            return;
        }

        try {

            //open csv in append mode
            int attribute = getAttribute();
            BufferedWriter writer = new BufferedWriter(new FileWriter(sampleDataFile, true));

            int trueXPrescaled = -1;
            int trueYPrescaled = -1;

            //System.out.println("Sample point data " + imageScaling + " " + sampledXTrue + " " + sampledYTrue + " " + trueXPrescaled + " " + trueYPrescaled);
            double x = -1.0;
            double y = -1.0;

            //write user, projectName, timestamp, x orig, y orig, x scaled, y scaled, state vector, attribute number, species, image file
            String data = sampleNumber + "\t" + user + "\t" + projectName + "\t" + time + "\t" + trueXPrescaled + "\t" + trueYPrescaled + "\t" + x + "\t" + y + "\t" + defaultImageWidth + "\t" + defaultLocalSize + "\t" + defaultWindowSize + "\t" + defaultOverviewSize + "\t" + state + attribute + "\t" + currentSpecies + "\t" + images[imageNumber] + "\n";
            writer.write(data);
            writer.close();

            this.setSampleNumber();
            savedState = true;

        } catch (IOException ex) {
            //Logger.getLogger(Frame_DataSampler.class.getName()).log(Level.SEVERE, null, ex);
            System.out.println("Error saving sample " + sampleNumber);
            return;
        }

        if (savedState) {
            System.out.println("Saved fast mode sample " + sampleNumber);
            this.notes = this.notes + "\nSaved fast mode sample " + sampleNumber + " for file " + images[imageNumber] + " with state " + state + " at time " + time + "\n";
            if (fnt != null) {
                fnt.updateNotes();
                //fnt.repaint();
            }
            this.setNextImage();
        }
        this.Panel_MainImage.repaint();
    }

    //write user, projectName, timestamp, x orig, y orig, x scaled, y scaled, state vector, attribute number, species, image file
    private void saveSample() {

        if (this.CheckBoxMenuItem_FastMode.isSelected()) {
            saveFastMode();
            return;
        } else {
            if (this.currentSample == null) {
                return;
            }

            onFirstSave();

            try {
                //to assure that the timestamp is unique for each sample, sleep for 5 milliseconds
                Thread.sleep(5);
            } catch (InterruptedException ex) {

            }
            long time = new Date().getTime();
            sampleNumber++;

            //String directory = this.rootDirectory + separator + defaultSamplesDirectory + separator + this.projectName;
            String directory = this.setSaveDirectory();

            //System.out.println(directory);
            //check if csv exists, and if not create it and write a header to it
            String sampleDataFile = directory + separator + dataFile;
            if (!Files.exists(Paths.get(sampleDataFile))) {
                try {
                    BufferedWriter writer = new BufferedWriter(new FileWriter(sampleDataFile));
                    String header = createHeader();
                    writer.write(header);
                    writer.close();
                    JOptionPane.showMessageDialog(this, "Creating data file " + sampleDataFile);

                } catch (IOException ex) {
                    Logger.getLogger(Frame_DataSampler.class
                            .getName()).log(Level.SEVERE, null, ex);
                }
            }

            boolean savedMontage = false;
            boolean savedPixel = false;
            boolean savedLocal = false;
            boolean savedWindow = false;
            boolean savedOverview = false;
            boolean savedState = false;

            String state = createStateString(); //includes trailing tab

            if (state == null) {
                JOptionPane.showMessageDialog(this, "<html>No characters were selected. At least one character must be selected. Not saving...<br>If none of the categories match, create a category for 'Other'.</html>");
                return;
            }

            try {

                //open csv in append mode
                int attribute = getAttribute();
                BufferedWriter writer = new BufferedWriter(new FileWriter(sampleDataFile, true));

                int trueXPrescaled = (int) ((double) this.sampledXTrue / (double) this.imageScaling);
                int trueYPrescaled = (int) ((double) this.sampledYTrue / (double) this.imageScaling);

                //System.out.println("Sample point data " + imageScaling + " " + sampledXTrue + " " + sampledYTrue + " " + trueXPrescaled + " " + trueYPrescaled);
                double x = ((double) sampledXTrue / (double) image.getWidth());
                double y = ((double) sampledYTrue / (double) image.getHeight());

                //write user, projectName, timestamp, x orig, y orig, x scaled, y scaled, state vector, attribute number, species, image file
                String data = sampleNumber + "\t" + user + "\t" + projectName + "\t" + time + "\t" + trueXPrescaled + "\t" + trueYPrescaled + "\t" + x + "\t" + y + "\t" + defaultImageWidth + "\t" + defaultLocalSize + "\t" + defaultWindowSize + "\t" + defaultOverviewSize + "\t" + state + attribute + "\t" + currentSpecies + "\t" + images[imageNumber] + "\n";
                writer.write(data);
                writer.close();

                this.insertSampledPixel(x, y);
                this.setSampleNumber();
                savedState = true;
                this.Panel_MainImage.repaint();

            } catch (IOException ex) {
                //Logger.getLogger(Frame_DataSampler.class.getName()).log(Level.SEVERE, null, ex);
                System.out.println("Error saving sample " + sampleNumber);
            }

            try {

                //String sampleDir = directory + separator + "sampleImages_" + sampleNumber + "." + projectName;
                //Files.createDirectories(Paths.get(sampleDir));
                BufferedImage pi = getSubimage(image, sampledXTrue, sampledYTrue, 1, 1);
                BufferedImage wi = getWindowImage(sampledXTrue, sampledYTrue, false);
                BufferedImage zoi = getZoomOutImage(sampledXTrue, sampledYTrue, true);
                BufferedImage li = getLocalImage(sampledXTrue, sampledYTrue, false);

                //write composited image file
                String fileImg = directory + separator + "Montage" + separator + "montage." + sampleNumber + "." + time + "." + defaultImageFormat;
                File outputfile = new File(fileImg);
                BufferedImage scaledMontage = scaleMontage();
                ImageIO.write(scaledMontage, defaultImageFormat, outputfile);
                savedMontage = true;

                //write pixel image file
                //if (pi == null) {
                //    System.out.println("Pi is null!" + sampledXTrue + " " + sampledYTrue);
                //}
                fileImg = directory + separator + "Pixel" + separator + "pixel." + sampleNumber + "." + time + "." + defaultImageFormat;
                outputfile = new File(fileImg);
                if (ImageIO.write(pi, defaultImageFormat, outputfile)) {
                    savedPixel = true;
                }
                //System.out.println("Saved pixel to " + fileImg);

                //write local image file
                fileImg = directory + separator + "Local" + separator + "local." + sampleNumber + "." + time + "." + defaultImageFormat;
                outputfile = new File(fileImg);
                ImageIO.write(li, defaultImageFormat, outputfile);
                savedLocal = true;

                //write window image file
                fileImg = directory + separator + "Window" + separator + "window." + sampleNumber + "." + time + "." + defaultImageFormat;
                outputfile = new File(fileImg);
                ImageIO.write(wi, defaultImageFormat, outputfile);
                savedWindow = true;

                //write overview image file
                fileImg = directory + separator + "Overview" + separator + "overview." + sampleNumber + "." + time + "." + defaultImageFormat;
                outputfile = new File(fileImg);
                zoi = scaleOverviewImage(zoi);
                ImageIO.write(zoi, defaultImageFormat, outputfile);
                savedOverview = true;

            } catch (IOException ex) {
                Logger.getLogger(Frame_DataSampler.class
                        .getName()).log(Level.SEVERE, null, ex);
            }

            if (savedState && savedMontage && savedPixel && savedLocal && savedWindow && savedOverview) {
                System.out.println("Saved sample " + sampleNumber);
                this.notes = this.notes + "\nSaved sample " + sampleNumber + " for file " + images[imageNumber] + "at time " + time + "\n";
                if (fnt != null) {
                    fnt.updateNotes();
                }
            }
        }
    }

    private int getAttribute() {
        if (this.ComboBox_AddedAttributeList == null) {
            return -1;
        }
        if (this.ComboBox_AddedAttributeList.getSelectedItem() == null) {
            return -1;
        }
        if (this.attributesHash == null) {
            return -1;
        }
        if (this.attributesHash.size() <= 0) {
            return -1;
        }

        String att = (String) this.ComboBox_AddedAttributeList.getSelectedItem();
        String tVal = (String) this.attributesHash.get(att);
        int val = Integer.parseInt(tVal);
        return val;
    }

    private void Button_SaveSampleMouseClicked(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_Button_SaveSampleMouseClicked
        if (this.Button_SaveSample.isEnabled()) {
            saveSample();
            if (this.CheckBox_ResetOnSave.isSelected()) {
                this.resetTableCharacters();
            }
            if (this.CheckBoxMenuItem_RandomizeSampling.isSelected() && this.CheckBoxMenuItem_RandomizeSampling.isEnabled()) {
                randomlySelectPixel();
            }
        }
    }//GEN-LAST:event_Button_SaveSampleMouseClicked

    private void Panel_ZoomImageComponentResized(java.awt.event.ComponentEvent evt) {//GEN-FIRST:event_Panel_ZoomImageComponentResized
        this.subImage = null;
        this.currentSample = null;
        //this.resetVariables(false, "ZoomImage panel");

        this.Panel_ZoomImage.repaint();
    }//GEN-LAST:event_Panel_ZoomImageComponentResized

    private void Panel_SampledImageComponentResized(java.awt.event.ComponentEvent evt) {//GEN-FIRST:event_Panel_SampledImageComponentResized
        System.out.println("resized sampled image");
//this.currentSample = null;
        //this.resetVariables(false, "resized Panel_SampledImage");
        //this.scaledImage = null;
        this.subImage = null;
        this.currentSample = null;
        this.Panel_SampledImage.repaint();
    }//GEN-LAST:event_Panel_SampledImageComponentResized

    private void CheckBoxMenuItem_RandomizeSamplingItemStateChanged(java.awt.event.ItemEvent evt) {//GEN-FIRST:event_CheckBoxMenuItem_RandomizeSamplingItemStateChanged
        randomSampling = this.CheckBoxMenuItem_RandomizeSampling.isSelected();
        if (this.CheckBoxMenuItem_RandomizeSampling.isSelected()) {
            this.CheckBoxMenuItem_Autosave.setSelected(false);
            this.CheckBoxMenuItem_Autosave.setEnabled(false);
            if (this.classification == null) {

                //JOptionPane.showMessageDialog(this, "Classifying pixels, please be patient. You will be alerted when complete. Push OK to continue...");
                classifyPixels(false);
                //JOptionPane.showMessageDialog(this, "Finished classifying pixels");

            }
            this.Button_RandomPixel.setEnabled(true);
            randomlySelectPixel();
            this.selectedXMiddle = -1;
            this.selectedYMiddle = -1;
            JOptionPane.showMessageDialog(this, "<html>Click 'Next Random Pixel' button.<br>Select character categories for the selected, highlighted pixel.<br>Click 'Save Selection' button to save sampled pixel.</html>");

        } else {
            this.resetVariables(false, "Randomize sampling menu");
            this.Button_RandomPixel.setEnabled(false);

        }
        this.checkSaveButton();
        //this.resetTableCharacters();
        this.repaint();
    }//GEN-LAST:event_CheckBoxMenuItem_RandomizeSamplingItemStateChanged

    private void Button_RandomPixelMouseClicked(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_Button_RandomPixelMouseClicked

        if (this.Button_RandomPixel.isEnabled()) {
            this.selectedXMiddle = -1;
            this.selectedYMiddle = -1;
            this.randomlySelectPixel();
        }
    }//GEN-LAST:event_Button_RandomPixelMouseClicked

    private void MenuItem_ImageInformationActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_MenuItem_ImageInformationActionPerformed

        if (this.imageFile == null) {
            dii = null;
            this.MenuItem_ImageInformation.setEnabled(false);
            return;
        }
        if (this.dii == null) {
            dii = new Dialog_ImageInformation(this, false, this.imageFile);

        }

        if (dii == null) {
            this.MenuItem_ImageInformation.setEnabled(false);
            return;
        }
        dii.setImagePath(this.imageFile);
        dii.pack();
        dii.setResizable(false);
        dii.setVisible(true);
    }//GEN-LAST:event_MenuItem_ImageInformationActionPerformed

    private void MenuItem_SamplingParametersActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_MenuItem_SamplingParametersActionPerformed
        if (fp == null) {
            this.fp = new Frame_Parameters(this);

        }
        fp.setVisible(true);
    }//GEN-LAST:event_MenuItem_SamplingParametersActionPerformed

    private void TextField_ZoomFactorKeyPressed(java.awt.event.KeyEvent evt) {//GEN-FIRST:event_TextField_ZoomFactorKeyPressed
        Frame_Parameters.setDoubleEditable(evt, this.TextField_ZoomFactor);
        setSliderValue();
    }//GEN-LAST:event_TextField_ZoomFactorKeyPressed

    private void setSliderValue() {
        double zoom = Double.parseDouble(this.TextField_ZoomFactor.getText());
        if (zoom < (double) this.Slider_Zoom.getMinimum() / 100.0) {
            zoom = (double) this.Slider_Zoom.getMinimum() / 100.0;
        }
        if (zoom > (double) this.Slider_Zoom.getMaximum() / 100.0) {
            zoom = (double) this.Slider_Zoom.getMaximum() / 100.0;
        }
        int value = (int) (zoom * 100.0);
        this.Slider_Zoom.setValue(value);

    }

    private void setSliderValue(int amountChange) {

        int curValue = this.Slider_Zoom.getValue();
        int newValue = curValue + amountChange;

        if (newValue < this.Slider_Zoom.getMinimum()) {
            newValue = this.Slider_Zoom.getMinimum();
        }

        if (newValue > this.Slider_Zoom.getMaximum()) {
            newValue = this.Slider_Zoom.getMaximum();
        }

        this.Slider_Zoom.setValue(newValue);

        double zoom = newValue / 100.0;

        this.TextField_ZoomFactor.setText(Double.toString(zoom));

    }

    private void TextField_LocalSizeKeyPressed(java.awt.event.KeyEvent evt) {//GEN-FIRST:event_TextField_LocalSizeKeyPressed
        Frame_Parameters.setIntegerEditable(evt, this.TextField_LocalSize);
    }//GEN-LAST:event_TextField_LocalSizeKeyPressed

    private void TextField_WindowSizeKeyPressed(java.awt.event.KeyEvent evt) {//GEN-FIRST:event_TextField_WindowSizeKeyPressed
        Frame_Parameters.setIntegerEditable(evt, this.TextField_WindowSize);
    }//GEN-LAST:event_TextField_WindowSizeKeyPressed

    private void TextField_OverviewSizeKeyPressed(java.awt.event.KeyEvent evt) {//GEN-FIRST:event_TextField_OverviewSizeKeyPressed
        Frame_Parameters.setIntegerEditable(evt, this.TextField_OverviewSize);
    }//GEN-LAST:event_TextField_OverviewSizeKeyPressed

    private void Button_ResetCharactersMouseClicked(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_Button_ResetCharactersMouseClicked
        resetTableCharacters();
    }//GEN-LAST:event_Button_ResetCharactersMouseClicked

    private void ComboBox_ImageFilesItemStateChanged(java.awt.event.ItemEvent evt) {//GEN-FIRST:event_ComboBox_ImageFilesItemStateChanged
        if (manualSelection) {
            String selectedFile = (String) this.ComboBox_ImageFiles.getSelectedItem();
            this.imageNumber = this.imageFilesHash.get(selectedFile);
            this.TextField_ImageNumber.setText(Integer.toString(imageNumber));
            //System.out.println("Calling from ComboBox_ImageFiles");
            setMainImage(this.rootDirectory + separator + defaultImagesDirectory + separator + images[imageNumber]);
        }
    }//GEN-LAST:event_ComboBox_ImageFilesItemStateChanged

    private void Panel_MainImageMouseEntered(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_Panel_MainImageMouseEntered
        trackMouse = true;
        this.selectedXMiddle = -1;
        this.selectedYMiddle = -1;
    }//GEN-LAST:event_Panel_MainImageMouseEntered

    private double getSliderZoom() {
        //return Math.log((double)this.Slider_Zoom.getValue() / 100.0);
        return (double) this.Slider_Zoom.getValue() / 100.0;
    }

    private void Slider_ZoomStateChanged(javax.swing.event.ChangeEvent evt) {//GEN-FIRST:event_Slider_ZoomStateChanged
        this.currentZoom = getSliderZoom();
        currentZoom = (double) Math.round((currentZoom) * 10) / 10;
        this.TextField_ZoomFactor.setText(Double.toString(currentZoom));

        this.setSelectedScaled(this.selectedXMiddle, this.selectedYMiddle, this.SLIDER_CHANGED);
        this.setSelectedScaled(this.selectedXMiddle, this.selectedYMiddle, this.MOUSE_MOVED);
        this.Panel_MainImage.repaint();

        if (this.CheckBoxMenuItem_RandomizeSampling.isSelected())
            this.randomlySelectPixel();
        else
            setSubimage();
    }//GEN-LAST:event_Slider_ZoomStateChanged

    private int checkImageNumber(int number) {
        if (number < 0) {
            number = 0;
            this.TextField_ImageNumber.setText(String.valueOf(number));
        } else if (number >= images.length) {
            number = images.length - 1;
            this.TextField_ImageNumber.setText(String.valueOf(number));
        }
        return number;
    }

    private void TextField_ImageNumberActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_TextField_ImageNumberActionPerformed
        String numberString = this.TextField_ImageNumber.getText();
        if (numberString != null) {
            //System.out.println("The number string is " + numberString);
            int number = Integer.parseInt(numberString);

            number = checkImageNumber(number);

            imageNumber = number;
            setFileComboBox(images[imageNumber]);
            //System.out.println("Calling from entered image number");
            setMainImage(this.rootDirectory + separator + defaultImagesDirectory + separator + images[imageNumber]);
        }
    }//GEN-LAST:event_TextField_ImageNumberActionPerformed

    private void TextField_ImageNumberKeyPressed(java.awt.event.KeyEvent evt) {//GEN-FIRST:event_TextField_ImageNumberKeyPressed
        Frame_Parameters.setIntegerEditable(evt, this.TextField_ImageNumber);
    }//GEN-LAST:event_TextField_ImageNumberKeyPressed

    private void MenuItem_ColorPaletteActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_MenuItem_ColorPaletteActionPerformed
        if (fsc == null) {
            this.fsc = new Frame_SelectionColors(this);
        }
        fsc.setVisible(true);
    }//GEN-LAST:event_MenuItem_ColorPaletteActionPerformed

    private void Panel_ZoomImageMouseDragged(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_Panel_ZoomImageMouseDragged
        /*
        if (this.CheckBoxMenuItem_RandomizeSampling.isSelected() && this.CheckBoxMenuItem_RandomizeSampling.isEnabled()) {
            JOptionPane.showMessageDialog(null, "<html>User sampling of pixels is disabled when random sampling is enabled. <br>Disable random sampling under the Sampling menu to sample pixels, or click 'Next Random Pixel'.</html>");
            return;
        }
        if (subImage == null || this.scaledImage == null) {
            this.Panel_SampledImage.setBackground(Color.white);
            return;
        }

        this.selectedXZoom = evt.getX();
        this.selectedYZoom = evt.getY();

        int x = (int) ((double) selectedXTrue + (double) selectedXZoom / (double) currentZoom);
        int y = (int) ((double) selectedYTrue + (double) selectedYZoom / (double) currentZoom);

        sampledXTrue = x;
        sampledYTrue = y;

        setSampleImage(sampledXTrue, sampledYTrue, true);
        checkSaveButton();
        //if (this.CheckBox_Autosave.isSelected()) {
        if (this.CheckBoxMenuItem_Autosave.isSelected() && this.CheckBoxMenuItem_Autosave.isEnabled()) {
            saveSample();
        }
        try {

            Graphics g1 = this.Panel_ZoomImage.getGraphics();
            g1.setColor(this.pointZoomColor);
            g1.drawRect(selectedXZoom, selectedYZoom, 1, 1);

        } catch (Exception e) {
        }
         */
    }//GEN-LAST:event_Panel_ZoomImageMouseDragged

    private void MenuItem_DeleteActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_MenuItem_DeleteActionPerformed
        if (fd == null) {
            fd = new Frame_Delete(this);
        }
        fd.setVisible(true);
    }//GEN-LAST:event_MenuItem_DeleteActionPerformed

    private void Panel_ZoomImageMouseWheelMoved(java.awt.event.MouseWheelEvent evt) {//GEN-FIRST:event_Panel_ZoomImageMouseWheelMoved
        int rotation = evt.getWheelRotation();
        setSliderValue(rotation * 10);
    }//GEN-LAST:event_Panel_ZoomImageMouseWheelMoved

    private void MenuItem_NotesActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_MenuItem_NotesActionPerformed
        if (fnt == null) {
            fnt = new Frame_NoteTaker(this);
        }
        fnt.setVisible(true);
        fnt.setAlwaysOnTop(true);
    }//GEN-LAST:event_MenuItem_NotesActionPerformed

    private void CheckBoxMenuItem_FastModeItemStateChanged(java.awt.event.ItemEvent evt) {//GEN-FIRST:event_CheckBoxMenuItem_FastModeItemStateChanged
        fastMode = this.CheckBoxMenuItem_FastMode.isSelected();
        if (fastMode) {
            this.CheckBox_ResetOnSave.setSelected(false);
            this.CheckBox_ResetOnSave.setEnabled(false);
            this.disableRandomAndAutosaveSampling();
            setImageStateHash(true);
        } else {
            this.CheckBox_ResetOnSave.setEnabled(true);
            this.enableRandomAndAutosaveSampling();
        }

        checkSaveButton();

    }//GEN-LAST:event_CheckBoxMenuItem_FastModeItemStateChanged

    private void CheckBoxMenuItem_AutosaveItemStateChanged(java.awt.event.ItemEvent evt) {//GEN-FIRST:event_CheckBoxMenuItem_AutosaveItemStateChanged
        autoSave = this.CheckBoxMenuItem_Autosave.isSelected();
        checkSaveButton();
    }//GEN-LAST:event_CheckBoxMenuItem_AutosaveItemStateChanged

    private void CheckBoxMenuItem_RandomizeSamplingActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_CheckBoxMenuItem_RandomizeSamplingActionPerformed
        // TODO add your handling code here:
    }//GEN-LAST:event_CheckBoxMenuItem_RandomizeSamplingActionPerformed

    private void Table_CharacterChecklistPropertyChange(java.beans.PropertyChangeEvent evt) {//GEN-FIRST:event_Table_CharacterChecklistPropertyChange
        // TODO add your handling code here:
    }//GEN-LAST:event_Table_CharacterChecklistPropertyChange

    private void Table_CharacterChecklistMouseClicked(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_Table_CharacterChecklistMouseClicked

    }//GEN-LAST:event_Table_CharacterChecklistMouseClicked

    private int getTableSelectedCount() {

        int numChar = this.Table_CharacterChecklist.getModel().getRowCount();
        int numSelected = 0;
        for (int i = 0; i <= numChar - 1; i++) {
            if (((Boolean) this.Table_CharacterChecklist.getModel().getValueAt(i, 2))) {
                numSelected++;
            }

        }
        return numSelected;
    }

    private void Button_SaveSampleActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_Button_SaveSampleActionPerformed
        // TODO add your handling code here:
    }//GEN-LAST:event_Button_SaveSampleActionPerformed

    private void Table_CharacterChecklistMousePressed(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_Table_CharacterChecklistMousePressed

    }//GEN-LAST:event_Table_CharacterChecklistMousePressed

    private void Table_CharacterChecklistMouseReleased(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_Table_CharacterChecklistMouseReleased
        if (this.CheckBox_MutuallyExclusive.isSelected()) {
            int row = Table_CharacterChecklist.rowAtPoint(evt.getPoint());
            int numChar = this.Table_CharacterChecklist.getModel().getRowCount();

            for (int i = 0; i <= numChar - 1; i++) {
                if (i != row) {
                    this.Table_CharacterChecklist.setValueAt(false, i, 2);
                }
            }
            numChar = getTableSelectedCount();
            if (numChar > 1) {
                JOptionPane.showMessageDialog(this, "Warning: Click not registered correctly.\nPlease ensure appropriate box is checked.");
            }
            //System.out.println("There are " + numChar + " characters selected.");
        }
    }//GEN-LAST:event_Table_CharacterChecklistMouseReleased

    private void classifyPixels(boolean replaceImage) {
        if (image == null) {
            return;
        }

        JOptionPane.showMessageDialog(this, "<html>Push OK to classify pixels for randomization. <br>Please be patient. You will be alterted when complete...</html>");

        // pb = new Frame_ProgressBar("Clustering pixels");
        //train on smaller image
        int smallSize = 500;
        BufferedImage small = scaleImage(image, smallSize, smallSize * image.getHeight() / image.getWidth(), BufferedImage.TYPE_INT_RGB, Image.SCALE_SMOOTH);
        int[] imagePixels = small.getRGB(0, 0, small.getWidth(), small.getHeight(), null, 0, small.getWidth());

        System.out.println("Training classifier");
        int[] means = Headless_KMeans.trainKMeans(imagePixels, this.kDefault, false);

        //classify on full image
        // pb.dispose();
        // pb = new Frame_ProgressBar("Classifying pixels");
        imagePixels = image.getRGB(0, 0, image.getWidth(), image.getHeight(), null, 0, image.getWidth());
        System.out.println("Classifying " + imagePixels.length + " pixels");
        this.classification = Headless_KMeans.classifyKMeans(means, imagePixels, replaceImage);
        System.out.println("Finished classification");

        if (replaceImage) {
            int w = image.getWidth();
            int h = image.getHeight();

            //if (w * h != imagePixels.length) {
            //System.out.println("Dimensions don't match: " + (w * h) + " vs " + imagePixels.length);
            //} else {
            //System.out.println("Dimensions match: " + (w * h) + " vs " + imagePixels.length);
            //}
            //image = new BufferedImage(w, h, BufferedImage.TYPE_INT_RGB);
            int[] bitMasks = new int[]{0xFF0000, 0xFF00, 0xFF, 0xFF000000};
            //int[] bitMasks = new int[]{0xFF0000, 0xFF00, 0xFF};
            SinglePixelPackedSampleModel sm = new SinglePixelPackedSampleModel(DataBuffer.TYPE_INT, w, h, bitMasks);
            DataBufferInt db = new DataBufferInt(imagePixels, imagePixels.length);
            WritableRaster wr = Raster.createWritableRaster(sm, db, new Point());
            image = new BufferedImage(ColorModel.getRGBdefault(), wr, false, null);

            //image.getRaster().setPixels(0, 0, w, h, imagePixels);
            this.Panel_MainImage.repaint();
        }

        JOptionPane.showMessageDialog(this, "Finished classifying pixels");
//pb.dispose();
    }

    private void insertSampledPixel(double x, double y) {
        //System.out.println("Attempting to insert sampled point");

        if (sampledPoints == null) {
            sampledPoints = new Hashtable<String, ArrayList<Point2D.Double>>(images.length);
        }
        if (images == null) {
            //System.out.println("Images is null");
            return;
        }
        if (images.length <= 0 || this.imageNumber < 0) {
            //System.out.println("Too few images " + images.length + " " + imageNumber);
            return;
        }

        String file = images[imageNumber];

        ArrayList<Point2D.Double> pts = sampledPoints.get(file);
        if (pts == null) {
            //System.out.println("No associated file with sampled points");
            pts = new ArrayList<Point2D.Double>();
        }

        pts.add(new Point2D.Double(x, y));
        sampledPoints.put(file, pts);

        //System.out.println("Inserted point (" + x + "," + y + ") for " + images[imageNumber]);
    }

    public int readSampledPixels(boolean reinitialize) {

        if (sampledPoints == null || reinitialize) {
            sampledPoints = new Hashtable<String, ArrayList<Point2D.Double>>(images.length);
        }

        int totPts = 0;

        String directory = rootDirectory + separator + defaultSamplesDirectory + separator + this.projectName;
        for (String type : types) {
            String sampleDataFile = directory + separator + type + separator + dataFile; //not user settable
            BufferedReader reader;
            try {
                reader = new BufferedReader(new FileReader(sampleDataFile));
                String line = reader.readLine(); // skip the header
                line = reader.readLine();
                while (line != null) {
                    String[] data = line.split("\\t", 0);
                    //if (data.length == 16) {
                    String file = data[data.length - 1]; //assumes filename is the last column
                    ArrayList<Point2D.Double> pts = sampledPoints.get(file);
                    if (pts == null) {
                        pts = new ArrayList<Point2D.Double>();
                    }
                    double x = Double.parseDouble(data[6]); //hardcoded = dangerous!
                    double y = Double.parseDouble(data[7]); //hardcoded = dangerous!
                    pts.add(new Point2D.Double(x, y));
                    sampledPoints.put(file, pts);
                    totPts++;
                    //}
                    line = reader.readLine();
                }
                reader.close();
            } catch (IOException e) {
                //System.out.println("No metadata file yet.");
            }
        }

        //Check fastmode points
        String sampleDataFile = directory + separator + dataFile; //not user settable
        BufferedReader reader;
        try {
            reader = new BufferedReader(new FileReader(sampleDataFile));
            String line = reader.readLine(); // skip the header
            line = reader.readLine();
            while (line != null) {
                String[] data = line.split("\\t", 0);
                //if (data.length == 16) {
                String file = data[data.length - 1]; //assumes filename is the last column
                ArrayList<Point2D.Double> pts = sampledPoints.get(file);
                if (pts == null) {
                    pts = new ArrayList<Point2D.Double>();
                }
                double x = 0.0; //hardcoded = dangerous!
                double y = 0.0; //hardcoded = dangerous!
                pts.add(new Point2D.Double(x, y));
                sampledPoints.put(file, pts);
                totPts++;
                //}
                line = reader.readLine();
            }
            reader.close();
        } catch (IOException e) {
            //System.out.println("No metadata file yet.");
        }

        return totPts;
    }

    private void randomlySelectPixel() {

        this.checkZoom();
        //uniformly select class
        int cat = rng.nextInt(this.kDefault);

        //uniformly randomly select pixel in class
        ArrayList<Integer> tal = this.classification.get(cat);
        int size = tal.size();
        int pixel = tal.get(rng.nextInt(size));
        //extract x, y pixel coordinates
        sampledXRandom = pixel % image.getWidth();
        sampledYRandom = pixel / image.getWidth();

        sampledXTrue = sampledXRandom;
        sampledYTrue = sampledYRandom;

        //System.out.println("Calling from randomlySelectPixel");
        setMainImage(sampledXRandom, sampledYRandom);
        setRandomPixelSubimage(sampledXRandom, sampledYRandom);
        setRandomPixelSample(sampledXRandom, sampledYRandom);
        resetTableCharacters();
    }

    private void setRandomPixelSample(int x, int y) {
        this.setSampleImage(x, y, true);
    }

    private void resetZoomPanels() {
        this.Panel_SampledImage.repaint();
        this.Panel_ZoomImage.repaint();
        this.selectedXScaled = -1;
        this.selectedYScaled = -1;
    }

    public void resetSampleNumber(int number) {
        sampleNumber = number;
        if (sampleNumber < 0) {
            sampleNumber = 0;
        }
        setSampleNumber();

    }

    private void setImageFileButtons() {
        if (images.length > 0) {
            this.Button_RandomImage.setEnabled(true);
        } else {
            this.Button_RandomImage.setEnabled(false);
        }
        if (imageNumber == 0) {
            this.Button_PreviousImageFile.setEnabled(false);
            if (imageNumber < images.length - 1) {
                this.Button_NextImageFile.setEnabled(true);
            }
        } else if (imageNumber > 0 && imageNumber < images.length - 1) {
            this.Button_PreviousImageFile.setEnabled(true);
            this.Button_NextImageFile.setEnabled(true);
        } else if (imageNumber == images.length - 1) {
            this.Button_NextImageFile.setEnabled(false);
            if (imageNumber > 0) {
                this.Button_PreviousImageFile.setEnabled(true);
            }
        }
    }

    public String getRootDirectory() {
        return this.rootDirectory;
    }

    public void setRootDirectory(String dir) {
        this.rootDirectory = dir;
    }

    public void createSamplePartitionMapping() {
        this.readSamplePartitionMapping();
        this.appendFilesToPartitionMapping();
        //writeSamplePartitionMappingToFile();
    }

    public void readSamplePartitionMapping() {
        int num;
        if (images == null) {

            num = 10;
        } else {
            num = images.length;
        }
        this.filePartitionType = new Hashtable<String, String>(num);

        try {
            String mappingFileLoc = rootDirectory + separator + defaultSamplesDirectory + separator + this.projectName + separator + mappingFile;
            BufferedReader br = new BufferedReader(new FileReader(mappingFileLoc));
            String line;

            while ((line = br.readLine()) != null) {
                String[] mapData = line.split("\\t");
                if (mapData.length == 2) {
                    filePartitionType.put(mapData[0], mapData[1]);
                }
            }
            br.close();
        } catch (Exception e) {
        }

    }

    public String randPMF(double[] prob, String[] out) {
        double rand = rng.nextDouble();
        int index = -1;
        while (rand >= 0.0) {
            index++;
            rand -= prob[index];
        }
        return out[index];
    }

    public void appendFilesToPartitionMapping() {

        if (images == null) {
            this.filePartitionType = new Hashtable<String, String>(10);
            return;
        }
        double[] probs = {(double) defaultTrainingPercentage / 100.0, (double) defaultTestingPercentage / 100.0, (double) defaultValidationPercentage / 100};

        for (String image : images) {

            if (!this.filePartitionType.containsKey(image)) {
                String type = randPMF(probs, types);
                this.filePartitionType.put(image, type);
            }
        }
    }

    public void writeSamplePartitionMappingToFile() {
        if (this.filePartitionType == null) {
            createSamplePartitionMapping();
        }
        String mappingFileLoc = rootDirectory + separator + defaultSamplesDirectory + separator + this.projectName + separator + mappingFile;
        //System.out.println("Writing partition mapping file to " + mappingFileLoc);
        try {
            BufferedWriter writer = new BufferedWriter(new FileWriter(mappingFileLoc));
            Set<String> setOfKeys = filePartitionType.keySet();

            // Iterating through the Hashtable
            // object using for-Each loop
            for (String key : setOfKeys) {
                String type = filePartitionType.get(key);
                writer.write(key + "\t" + type + "\n");
                //System.out.println(key + "\t" + type + "\n");
            }
            writer.close();

        } catch (IOException ex) {
            ex.printStackTrace();
            Logger
                    .getLogger(Frame_DataSampler.class
                            .getName()).log(Level.SEVERE, null, ex);
        }
    }

    public void readAndCreateSamplePartitionMapping(boolean createMappingFile) {

        String sampleRoot = rootDirectory + separator + defaultSamplesDirectory + separator + this.projectName;
        if (Files.exists(Paths.get(sampleRoot + separator + mappingFile))) {
            readSamplePartitionMapping();

        } else if (this.filePartitionType == null) {
            int num;
            if (images == null) {
                num = 10;
            } else {
                num = images.length;
            }
            this.filePartitionType = new Hashtable<String, String>(num);
        }
        appendFilesToPartitionMapping();
        if (createMappingFile) {
            writeSamplePartitionMappingToFile();
        }

    }

    public boolean checkPriorPartitioningScheme(boolean createPartitionFile) {
        String sampleRoot = rootDirectory + separator + defaultSamplesDirectory + separator + this.projectName;
        if (Files.exists(Paths.get(sampleRoot + separator + mappingFile))) {
            this.partitionByFile = true;
            this.disablePartitionTypeSelection = true;
            readAndCreateSamplePartitionMapping(false);
        } else if (Files.exists(Paths.get(sampleRoot + separator + "Training" + separator + dataFile))) {
            this.partitionByFile = false;
            this.disablePartitionTypeSelection = true;
        } else if (Files.exists(Paths.get(sampleRoot + separator + "Testing" + separator + dataFile))) {
            this.partitionByFile = false;
            this.disablePartitionTypeSelection = true;
        } else if (Files.exists(Paths.get(sampleRoot + separator + "Validation" + separator + dataFile))) {
            this.partitionByFile = false;
            this.disablePartitionTypeSelection = true;
        } else {
            this.disablePartitionTypeSelection = false;
            if (this.partitionByFile) {
                readAndCreateSamplePartitionMapping(createPartitionFile);
            }
        }

        //if (fp == null) {
        //    fp = new Frame_Parameters(this);
        //}
        //fp.setPartitionEnabling();
        return this.partitionByFile;
    }

    /**
     * @param args the command line arguments
     */
    public static void main(String args[]) {

        //System.out.println("Heap size: " + Runtime.getRuntime().totalMemory());
        /* Set the Nimbus look and feel */
        //<editor-fold defaultstate="collapsed" desc=" Look and feel setting code (optional) ">
        /* If Nimbus (introduced in Java SE 6) is not available, stay with the default look and feel.
         * For details see http://download.oracle.com/javase/tutorial/uiswing/lookandfeel/plaf.html
         */
        try {
            for (javax.swing.UIManager.LookAndFeelInfo info : javax.swing.UIManager.getInstalledLookAndFeels()) {
                if ("Nimbus".equals(info.getName())) {
                    javax.swing.UIManager.setLookAndFeel(info.getClassName());
                    break;

                }
            }
        } catch (ClassNotFoundException ex) {
            java.util.logging.Logger.getLogger(Frame_DataSampler.class
                    .getName()).log(java.util.logging.Level.SEVERE, null, ex);

        } catch (InstantiationException ex) {
            java.util.logging.Logger.getLogger(Frame_DataSampler.class
                    .getName()).log(java.util.logging.Level.SEVERE, null, ex);

        } catch (IllegalAccessException ex) {
            java.util.logging.Logger.getLogger(Frame_DataSampler.class
                    .getName()).log(java.util.logging.Level.SEVERE, null, ex);

        } catch (javax.swing.UnsupportedLookAndFeelException ex) {
            java.util.logging.Logger.getLogger(Frame_DataSampler.class
                    .getName()).log(java.util.logging.Level.SEVERE, null, ex);
        }
        //</editor-fold>
        //</editor-fold>
        //</editor-fold>
        //</editor-fold>

        /* Create and display the form */
        java.awt.EventQueue.invokeLater(() -> {

            Frame_DataSampler fds = new Frame_DataSampler();
            Frame_SetUserInfo fsui = new Frame_SetUserInfo(fds);

            fsui.setVisible(true);
            //fds.setVisible(true);
            //fds.initializeDataFromDefaults();
        });
    }

    static int MOUSE_MOVED = 1;
    static int MOUSE_PRESSED = 2;
    static int SLIDER_CHANGED = 3;

    public Color rectangleSelectedColor = Color.BLUE;
    public Color rectangleRandomColor = Color.RED;
    public Color pointSelectedColor = Color.YELLOW;
    public Color pointRandomColor = Color.YELLOW;
    public Color pointZoomColor = Color.RED;
    public Color sampledPointsColor = Color.CYAN;

    public BufferedImage image = null;
    public BufferedImage scaledImage;
    public BufferedImage subImage;
    public BufferedImage zoomImage;
    public BufferedImage currentSample;

    int defaultImageWidth = 700;

    String separator = File.separator;
    public int sampleNumber = 0;
    public double defaultZoom = 5;
    public double currentZoom = defaultZoom;
    public int defaultWindowSize = defaultImageWidth / 100 + 1;
    public int defaultOverviewSize = defaultImageWidth / 4 + 1;
    //public int finalOverviewSize = 200;
    public int defaultLocalSize = 5;
    public int selectedXScaled = -1, selectedYScaled = -1;
    public int selectedXTrue, selectedYTrue;
    public int sampledXTrue = -1, sampledYTrue = -1;
    public int sampledXRandom = -1, sampledYRandom = -1;
    public int selectedXMiddle = -1, selectedYMiddle = -1;
    public int selectedXZoom = -1, selectedYZoom = -1;

    private int scaledImageW = -1;
    private int scaledImageH = -1;

    private int mousePositionX = -1;
    private int mousePositionY = -1;
    private int showW = -1;
    private int showH = -1;

    boolean trackMouse = true;

    private double imageScaling = -1.0;

    public int savedMontageSize = 250;
    public int savedOverviewSize = 100;

    public String defaultImageFormat = "png";

    private int mainPanelW = -1;
    private int mainPanelH = -1;

    private Frame_SelectCharactersFileBrowser scfb;
    private Frame_SelectImagesDirectory sid;
    private Frame_SelectAdditionalAttributeFileBrowser saafb;
    private Frame_Parameters fp;
    private Frame_SelectionColors fsc;
    private Frame_Delete fd;
    private Frame_NoteTaker fnt;

    public Dialog_ImageInformation dii;

    //private Dialog_OK OKDialog;
    public String characterFile;
    public String attributesFile;
    public String imageFile;
    public String currentSpecies;

    //relative to root
    public String defaultImagesDirectory = "Images";
    public String defaultCharactersDirectory = "Characters";
    public String defaultAttributesDirectory = "Attributes";
    public String defaultCharactersFile = "Characters.txt";
    public String defaultAttributesFile = "Attributes.txt";
    public String defaultSamplesDirectory = "Samples";
    public String rootDirectory = "D:\\Projects\\HerbariumDNN";
    public String mappingFile = "fileSampleTypeMapping.txt";
    public String dataFile = "sampleMetadata.txt";
    public String projectParametersFile = "projectParameters.txt";
    public String notesFile = "projectNotes.txt";

    public String characterSplitString = "-";
    //public String rootDirectory = "";

    public int defaultTrainingPercentage = 60;
    public int defaultTestingPercentage = 20;
    public int defaultValidationPercentage = 20;

    public boolean disablePartitionTypeSelection = false;
    public boolean partitionByFile = true;

    String[] types = {"Training", "Testing", "Validation"};
    String[] imageTypes = {"Pixel", "Local", "Window", "Overview", "Montage"};

    ArrayList<String> characters;
    Hashtable<String, Boolean> charactersHash;
    Hashtable<String, String> attributesHash;
    Hashtable<String, Integer> imageFilesHash;
    Hashtable<String, String> propertiesHash;
    Hashtable<String, String> filePartitionType;
    Hashtable<Integer, String> headerHash;
    Hashtable<String, String> imageStateHash;

    String[] images;
    Hashtable<Integer, ArrayList<Integer>> classification;
    Hashtable<String, ArrayList<Point2D.Double>> sampledPoints; //samples by file (String), saved points (array list of relative positions of sampled points)
    int kDefault = 6;

    String projectName = "default";
    String user = "default user";
    String notes = "";

    boolean imagesEnabled = false;
    boolean attributesEnabled = false;
    boolean charactersEnabled = false;

    boolean manualSelection = true;

    boolean fastMode = false;
    boolean autoSave = false;
    boolean randomSampling = false;
    boolean resetOnSave = true;

    //
    int imageNumber;

    Random rng;

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton Button_NextImageFile;
    private javax.swing.JButton Button_PreviousImageFile;
    private javax.swing.JButton Button_RandomImage;
    private javax.swing.JButton Button_RandomPixel;
    private javax.swing.JButton Button_ResetCharacters;
    private javax.swing.JButton Button_SaveSample;
    private javax.swing.JCheckBoxMenuItem CheckBoxMenuItem_Autosave;
    private javax.swing.JCheckBoxMenuItem CheckBoxMenuItem_FastMode;
    private javax.swing.JCheckBoxMenuItem CheckBoxMenuItem_RandomizeSampling;
    private javax.swing.JCheckBox CheckBox_MutuallyExclusive;
    private javax.swing.JCheckBox CheckBox_ResetOnSave;
    private javax.swing.JComboBox<String> ComboBox_AddedAttributeList;
    private javax.swing.JComboBox<String> ComboBox_ImageFiles;
    private javax.swing.JLabel Label_AddedAttribute;
    private javax.swing.JLabel Label_ClickMainImage;
    private javax.swing.JLabel Label_ClickZoomedImage;
    private javax.swing.JLabel Label_LocalSize;
    private javax.swing.JLabel Label_MainImage;
    private javax.swing.JLabel Label_OverviewSize;
    private javax.swing.JLabel Label_SampleNumber;
    private javax.swing.JLabel Label_WindowSize;
    private javax.swing.JMenuItem MenuItem_ColorPalette;
    private javax.swing.JMenuItem MenuItem_Delete;
    private javax.swing.JMenuItem MenuItem_Exit;
    private javax.swing.JMenuItem MenuItem_ImageInformation;
    private javax.swing.JMenuItem MenuItem_Notes;
    private javax.swing.JMenuItem MenuItem_OpenAddedAttributeFile;
    private javax.swing.JMenuItem MenuItem_OpenCharactersFile;
    private javax.swing.JMenuItem MenuItem_OpenImagesDirectory;
    private javax.swing.JMenuItem MenuItem_SamplingParameters;
    private javax.swing.JMenu Menu_File;
    private javax.swing.JMenu Menu_Help;
    private javax.swing.JMenu Menu_Log;
    private javax.swing.JMenuBar Menu_MainMenu;
    private javax.swing.JMenu Menu_Parameters;
    private javax.swing.JMenu Menu_Sampling;
    private javax.swing.JScrollPane Panel_CharacterCheckList;
    private javax.swing.JPanel Panel_MainImage;
    private javax.swing.JPanel Panel_SampledImage;
    private javax.swing.JPanel Panel_ZoomImage;
    private javax.swing.JSlider Slider_Zoom;
    private javax.swing.JTable Table_CharacterChecklist;
    private javax.swing.JTextField TextField_ImageNumber;
    private javax.swing.JTextField TextField_LocalSize;
    private javax.swing.JTextField TextField_OverviewSize;
    private javax.swing.JTextField TextField_WindowSize;
    private javax.swing.JTextField TextField_ZoomFactor;
    private javax.swing.JMenuItem jMenuItem1;
    // End of variables declaration//GEN-END:variables

}
