package ModelInterface;

import java.util.*;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import javax.swing.JFrame;
import javax.swing.UIManager;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JMenuBar;
import javax.swing.JOptionPane;
import javax.swing.KeyStroke;

import javax.swing.undo.UndoManager;
import javax.swing.undo.CannotUndoException;
import javax.swing.undo.CannotRedoException;

import org.w3c.dom.Node;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

import ModelInterface.ConfigurationEditor.configurationeditor.ConfigurationEditor;
import ModelInterface.DMsource.DMViewer;
import ModelInterface.ModelGUI2.DbViewer;
import ModelInterface.ModelGUI2.InputViewer;
import ModelInterface.PPsource.PPViewer;
import ModelInterface.common.RecentFilesList;

import ModelInterface.ModelGUI2.XMLFilter;
import ModelInterface.ConfigurationEditor.utils.FileUtils;
import ModelInterface.common.FileChooser;
import ModelInterface.common.FileChooserFactory;
import ModelInterface.BatchRunner;

import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.event.KeyEvent;
import java.awt.Container;
import java.awt.BorderLayout;
import java.awt.Cursor;

public class InterfaceMain extends JFrame implements ActionListener {
	/**
	 * Unique identifier used for serializing.
	 */
	private static final long serialVersionUID = -9137748180688015902L;

	public static final int FILE_MENU_POS = 0;
	public static final int EDIT_MENU_POS = 1;
	public static final int FILE_NEW_MENUITEM_POS = 0;
	public static final int FILE_OPEN_SUBMENU_POS = 5;
	public static final int FILE_SAVE_MENUITEM_POS = 10;
	public static final int FILE_SAVEAS_MENUITEM_POS = 11;
	public static final int FILE_QUIT_MENUITEM_POS = 50;
	public static final int EDIT_COPY_MENUITEM_POS = 10;
	public static final int EDIT_PASTE_MENUITEM_POS = 11;
	public static final int EDIT_UNDO_MENUITEM_POS = 1;
	public static final int EDIT_REDO_MENUITEM_POS = 2;

	private static File propertiesFile = new File("model_interface.properties");
	private static String oldControl;
	private static InterfaceMain main;
	private JMenuItem newMenu;
	private JMenuItem saveMenu;
	private JMenuItem saveAsMenu;
	private JMenuItem quitMenu;
	private JMenuItem copyMenu;
	private JMenuItem pasteMenu;
	private JMenuItem undoMenu;
	private JMenuItem redoMenu;
	private JMenuItem batchMenu;
	private Properties savedProperties;

	private UndoManager undoManager;

	private List<MenuAdder> menuAdders;

	/**
	 * Main function, creates a new thread for the gui and runs it.
	 */
	public static void main(String[] args) {
		System.out.println("Library Path: "+System.getProperty("java.library.path"));

		//Schedule a job for the event-dispatching thread:
		//creating and showing this application's GUI.

		try {
			UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
		} catch (Exception e) {
			// warn the user.. should be ok to keep going
			System.out.println("Error setting look and feel: " + e);
		}
		/* does seem to work for ^C or end tasks..
		Runtime.getRuntime().addShutdownHook(new Thread() {
			public void run() {
				System.out.println("IS this even running");
				javax.swing.SwingUtilities.invokeLater(new Runnable() {
					public void run() {
						main.fireControlChange("InterfaceMain");
					}
				});
			}
		});
		*/

		Thread.setDefaultUncaughtExceptionHandler(new Thread.UncaughtExceptionHandler() {
			public void uncaughtException(Thread t, Throwable e) {
				JOptionPane.showMessageDialog(null, e, "Unexpected Error", 
					JOptionPane.ERROR_MESSAGE);
				// still print the stack trace to the console for debugging
				e.printStackTrace();
			}
		});

		javax.swing.SwingUtilities.invokeLater(new Runnable() {
			public void run() {
				createAndShowGUI();
			}
		});

	}

	/**
	 * Create a new instance of this class and makes it visible
	 */
	private static void createAndShowGUI() {
		main = null;
		main  = new InterfaceMain("Model Interface");
		main.initialize();
		//main.pack();
		main.setVisible(true);
	}

	private InterfaceMain(String title) {
		super(title);
		savedProperties = new Properties();
		if(propertiesFile.exists()) {
			try {
				savedProperties.loadFromXML(new FileInputStream(propertiesFile));
			} catch (FileNotFoundException notFound) {
				// well I checked if it existed before so..
				System.out.println("Wow you made it get here, you win 1 million dollars...");
				System.out.println("Ask James for you prize");
			} catch (IOException ioe) {
				ioe.printStackTrace();
			}
		}
		if(Boolean.parseBoolean(savedProperties.getProperty("isMaximized", "false"))) {
			setExtendedState(MAXIMIZED_BOTH);
		}
		String lastHeight = savedProperties.getProperty("lastHeight", "600");
		String lastWidth = savedProperties.getProperty("lastWidth", "800");
		setSize(Integer.parseInt(lastWidth), Integer.parseInt(lastHeight));
		Container contentPane = getContentPane();

		contentPane.setLayout(new BorderLayout());

		oldControl = "ModelInterface";
	}

	private void initialize() {
		MenuManager menuMan = new MenuManager(null);
		addWindowAdapters();
		addMenuItems(menuMan);
		addMenuAdderMenuItems(menuMan);
		finalizeMenu(menuMan);
	}
	
	private void addMenuItems(MenuManager menuMan) {
		JMenu m = new JMenu("File");
		menuMan.addMenuItem(m, FILE_MENU_POS);
		JMenu submenu;
		JMenuItem menuItem;

		submenu = new JMenu("Open");
		submenu.setMnemonic(KeyEvent.VK_S);
		menuMan.getSubMenuManager(FILE_MENU_POS).addMenuItem(submenu, FILE_OPEN_SUBMENU_POS);
		menuMan.getSubMenuManager(FILE_MENU_POS).addSeparator(FILE_OPEN_SUBMENU_POS+2);
		//m.add(submenu);
		//m.addSeparator();

		//m.add(makeMenuItem("Quit"));
		menuMan.getSubMenuManager(FILE_MENU_POS).addMenuItem(newMenu = new JMenuItem("New"), FILE_NEW_MENUITEM_POS);
		menuMan.getSubMenuManager(FILE_MENU_POS).addSeparator(FILE_NEW_MENUITEM_POS);
		newMenu.setEnabled(false);
		menuMan.getSubMenuManager(FILE_MENU_POS).addMenuItem(saveMenu = new JMenuItem("Save")/*makeMenuItem("Save")*/, FILE_SAVE_MENUITEM_POS);
		saveMenu.setEnabled(false);
		menuMan.getSubMenuManager(FILE_MENU_POS).addMenuItem(saveAsMenu = new JMenuItem("Save As"), FILE_SAVEAS_MENUITEM_POS);
		menuMan.getSubMenuManager(FILE_MENU_POS).addSeparator(FILE_SAVEAS_MENUITEM_POS);
		saveAsMenu.setEnabled(false);
		menuMan.getSubMenuManager(FILE_MENU_POS).addMenuItem(quitMenu = makeMenuItem("Quit"), FILE_QUIT_MENUITEM_POS);

		menuMan.addMenuItem(new JMenu("Edit"), EDIT_MENU_POS);

		copyMenu = new JMenuItem("Copy");
		// key stroke is system dependent
		//copyMenu.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_C, ActionEvent.CTRL_MASK));
		menuMan.getSubMenuManager(EDIT_MENU_POS).addMenuItem(copyMenu, EDIT_COPY_MENUITEM_POS);
		pasteMenu = new JMenuItem("Paste");
		// key stroke is system dependent
		//pasteMenu.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_V, ActionEvent.CTRL_MASK));
		menuMan.getSubMenuManager(EDIT_MENU_POS).addMenuItem(pasteMenu, EDIT_PASTE_MENUITEM_POS);
		menuMan.getSubMenuManager(EDIT_MENU_POS).addSeparator(EDIT_PASTE_MENUITEM_POS);

		copyMenu.setEnabled(false);
		pasteMenu.setEnabled(false);

		batchMenu = new JMenuItem("Batch File");
		batchMenu.setEnabled(true);
		batchMenu.addActionListener(this);
		menuMan.getSubMenuManager(FILE_MENU_POS).addMenuItem(batchMenu, FILE_OPEN_SUBMENU_POS);

		setupUndo(menuMan);
	}

	private void addMenuAdderMenuItems(MenuManager menuMan) {
		/* FileChooserDemo is being removed, but I will leave this here,
		 * This is how I envision the menuitems to be added and hopefully all
		 * the listeners would be set up correctly and we won't need to keep
		 * the pointer to the classes around
		 FileChooserDemo fcd = new FileChooserDemo(this);
		 fcd.addMenuItems(menuMan);
		 */
		final MenuAdder dbView = new DbViewer(this);
		dbView.addMenuItems(menuMan);
		final MenuAdder inputView = new InputViewer(this);
		inputView.addMenuItems(menuMan);
		final MenuAdder PPView = new PPViewer(this);
		PPView.addMenuItems(menuMan);
		final MenuAdder DMView = new DMViewer(this);
		DMView.addMenuItems(menuMan);
		final MenuAdder recentFilesList = RecentFilesList.getInstance();
		recentFilesList.addMenuItems(menuMan);

		// Create the Configuration editor and allow it to add its menu items to the
		// menu system.
		final MenuAdder confEditor = new ConfigurationEditor();
		confEditor.addMenuItems(menuMan);
		
		menuAdders = new ArrayList<MenuAdder>(6);
		menuAdders.add(dbView);
		menuAdders.add(inputView);
		menuAdders.add(PPView);
		menuAdders.add(DMView);
		menuAdders.add(recentFilesList);
		menuAdders.add(confEditor);
	}

	private void finalizeMenu(MenuManager menuMan) {
		JMenuBar mb = menuMan.createMenu(); //new JMenuBar();
		setJMenuBar(mb);
	}

	private void addWindowAdapters() {
		// Add adapter to catch window events.
		WindowAdapter myWindowAdapter = new WindowAdapter() {
			public void windowStateChanged(WindowEvent e) {
				savedProperties.setProperty("isMaximized", String.valueOf((e.getNewState() & MAXIMIZED_BOTH) != 0));
			}
			public void windowClosing(WindowEvent e) {
				System.out.println("Caught the window closing");
				firePropertyChange("Control", oldControl, "ModelInterface");
				if(!Boolean.parseBoolean(savedProperties.getProperty("isMaximized"))) {
					savedProperties.setProperty("lastWidth", String.valueOf(getWidth()));
					savedProperties.setProperty("lastHeight", String.valueOf(getHeight()));
				}
				try {
					savedProperties.storeToXML(new FileOutputStream(propertiesFile), "TODO: add comments");
				} catch(FileNotFoundException notFound) {
					notFound.printStackTrace();
				} catch (IOException ioe) {
					ioe.printStackTrace();
				}
				System.exit(0);
			}
			public void windowClosed(WindowEvent e) {
				System.out.println("Caught the window closed");
				firePropertyChange("Control", oldControl, "ModelInterface");
				if(!Boolean.parseBoolean(savedProperties.getProperty("isMaximized"))) {
					savedProperties.setProperty("lastWidth", String.valueOf(getWidth()));
					savedProperties.setProperty("lastHeight", String.valueOf(getHeight()));
				}
				try {
					savedProperties.storeToXML(new FileOutputStream(propertiesFile), "TODO: add comments");
				} catch(FileNotFoundException notFound) {
					notFound.printStackTrace();
				} catch (IOException ioe) {
					ioe.printStackTrace();
				}
				System.exit(0);
			}
		};
		addWindowListener(myWindowAdapter);
		addWindowStateListener(myWindowAdapter);

		getGlassPane().addMouseListener( new MouseAdapter() {});
		getGlassPane().setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
	}

	private JMenuItem makeMenuItem(String title) {
		JMenuItem m = new JMenuItem(title);
		m.addActionListener(this);
		return m;
	}

	public void actionPerformed(ActionEvent e) {
		if(e.getActionCommand().equals("Quit")) {
			//firePropertyChange("Control", oldControl, "ModelInterface");
			dispose();
		} else if(e.getActionCommand().equals("Batch File")) {
			// TODO: make it so recent files could work with this
			FileChooser fc = FileChooserFactory.getFileChooser();
			final File[] result = fc.doFilePrompt(this, "Open Batch File", FileChooser.LOAD_DIALOG, 
					new File(getProperties().getProperty("lastDirectory", ".")),
					new XMLFilter());
            // these should be run off the GUI thread
            new Thread(new Runnable() {
                public void run() {
                    if(result != null) {
                        for(File file : result) {
                            runBatch(file);
                        }
                    }
                    // TODO: message that all were run
                }
            }).start();
		}
	}
	public static InterfaceMain getInstance() {
		return main;
	}
	public JMenuItem getNewMenu() {
		return newMenu;
	}
	public JMenuItem getSaveMenu() {
		return saveMenu;
	}
	public JMenuItem getSaveAsMenu() {
		return saveAsMenu;
	}
	public JMenuItem getQuitMenu() {
		return quitMenu;
	}
	public JMenuItem getCopyMenu() {
		return copyMenu;
	}
	public JMenuItem getPasteMenu() {
		return pasteMenu;
	}
	public JMenuItem getUndoMenu() {
		// will this be needed since they will be setup in here?
		return undoMenu;
	}
	public JMenuItem getRedoMenu() {
		// will this be needed since they will be setup in here?
		return redoMenu;
	}
	public JMenuItem getBatchMenu() {
		return batchMenu;
	}
	public void fireControlChange(String newValue) {
		System.out.println("Going to change controls");
		if(newValue.equals(oldControl)) {
			oldControl += "Same";
		}
		firePropertyChange("Control", oldControl, newValue);
		oldControl = newValue;
	}
	public void fireProperty(String propertyName, Object oldValue, Object newValue) {
		firePropertyChange(propertyName, oldValue, newValue);
	}
	public class MenuManager {
		private JMenuItem menuValue;
		private Map subItems;
		private SortedSet sepList;
		MenuManager(JMenuItem menuValue) {
			this.menuValue = menuValue;
			sepList = null;
			if(menuValue == null || menuValue instanceof JMenu) {
				subItems = new TreeMap();
			} else {
				subItems = null;
			}
		}
		/*
		public JMenuItem getMenuValue() {
			return menuValue;
		}
		public Map getSubItems() {
			return subItems;
		}
		*/
		public void addSeparator(int where) {
			if(sepList == null) {
				sepList = new TreeSet();
			}
			sepList.add(where);
		}
		public int addMenuItem(JMenuItem menu, int where) {
			if(subItems.containsKey(where)) {
				return addMenuItem(menu, where+1);
			} else {
				subItems.put(where, new MenuManager(menu));
				return where;
			}
		}
		public MenuManager getSubMenuManager(int where) {
			if(!subItems.containsKey(where)) {
				// throw exception or just return null?
				return null;
			}
			return ((MenuManager)subItems.get(where));
		}
		JMenuBar createMenu() {
			JMenuBar ret = new JMenuBar();
			Object[] keys = subItems.keySet().toArray();
			for(int i = 0; i < keys.length; ++i) {
				ret.add(((MenuManager)subItems.get(keys[i])).createSubMenu());
			}
			return ret;
		}
		private JMenuItem createSubMenu() {
			if(subItems == null) {
				return menuValue;
			} else {
				Object[] keys = subItems.keySet().toArray();
				for(int i = 0; i < keys.length; ++i) {
					if(sepList != null && !sepList.isEmpty() &&
							((Integer)keys[i]).intValue() > ((Integer)sepList.first()).intValue()) {
						((JMenu)menuValue).addSeparator();
						sepList.remove(sepList.first());
					}
					menuValue.add(((MenuManager)subItems.get(keys[i])).createSubMenu());
				}
				return menuValue;
			}
		}
	}
	public Properties getProperties() {
		return savedProperties;
	}
	private void setupUndo(MenuManager menuMan) {
		undoManager = new UndoManager();
		undoManager.setLimit(10);

		undoMenu = new JMenuItem("Undo");
		menuMan.getSubMenuManager(EDIT_MENU_POS).addMenuItem(undoMenu, EDIT_UNDO_MENUITEM_POS);
		redoMenu = new JMenuItem("Redo");
		menuMan.getSubMenuManager(EDIT_MENU_POS).addMenuItem(redoMenu, EDIT_REDO_MENUITEM_POS);
		menuMan.getSubMenuManager(EDIT_MENU_POS).addSeparator(EDIT_REDO_MENUITEM_POS);

		undoMenu.setEnabled(false);
		redoMenu.setEnabled(false);

		ActionListener undoListener = new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				String cmd = e.getActionCommand();
				if(cmd.startsWith("Undo")) {
					try {
						undoManager.undo();
						refreshUndoRedo();
					} catch(CannotUndoException cue) {
						cue.printStackTrace();
					}
				} else if(cmd.startsWith("Redo")) {
					try {
						undoManager.redo();
						refreshUndoRedo();
					} catch(CannotRedoException cre) {
						cre.printStackTrace();
					}
				} else {
					System.out.println("Didn't recognize: "+cmd);
				}
			}
		};

		undoMenu.addActionListener(undoListener);
		redoMenu.addActionListener(undoListener);
	}
	public UndoManager getUndoManager() {
		return undoManager;
	}
	public void refreshUndoRedo() {
		undoMenu.setText(undoManager.getUndoPresentationName());
		undoMenu.setEnabled(undoManager.canUndo());
		redoMenu.setText(undoManager.getRedoPresentationName());
		redoMenu.setEnabled(undoManager.canRedo());
	}
	/**
	 * Get the menu adder with the specified class name.  Used
	 * to get the instance of the menu adder that could open
	 * a recent file.
	 * @param classname The class that is requested.
	 * @return The instance of the class or null if not found.
	 */ 
	public MenuAdder getMenuAdder(String classname) {
		for(Iterator<MenuAdder> it = menuAdders.iterator(); it.hasNext(); ) {
			MenuAdder curr = it.next();
			if(curr.getClass().getName().equals(classname)) {
				return curr;
			}
		}
		return null;
	}
	/**
	 * Runs the given batch file.  Relys on the menuAdders list
	 * and if any of the class implements BatchRunner it will pass
	 * it off the command to that class. 
	 * @param file The batch file to run.
	 * @see BatchRunner 
	 */
	private void runBatch(File file) {
		// don't really care about document element's name
		Node doc = FileUtils.loadDocument(this, file, null).getDocumentElement();

		// TODO: remove this check once batch queries get merged
		if(doc.getNodeName().equals("queries")) {
			System.out.println("Batch queries are not yet merged with this functionality.");
			System.out.println("Please open a database then run the batch file.");
			// TODO: print this on the screen
			return;
		}

		NodeList commands = doc.getChildNodes();
		for(int i = 0; i < commands.getLength(); ++i) {
			if(commands.item(i).getNodeName().equals("class")) {
				Element currClass = (Element)commands.item(i);
				String className = currClass.getAttribute("name");
				MenuAdder runner = getMenuAdder(className);
				if(runner != null && runner instanceof BatchRunner) {
					((BatchRunner)runner).runBatch(currClass);
				} else {
					System.out.println("could not find batch runner for class "+className);
					JOptionPane.showMessageDialog(this,
							"Could not find batch runner for class "+className,
							"Batch File Error", JOptionPane.ERROR_MESSAGE);
				}
			}
		}
		System.out.println("Finished running "+file);
		JOptionPane.showMessageDialog(this,
				"Finished running batch file "+file.getName(),
				"Batch File Complete", JOptionPane.INFORMATION_MESSAGE);
	}
}