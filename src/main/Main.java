package main;


import org.json.JSONArray;
import org.json.JSONObject;

import solver.TaskSolver;
import units.SourceStream;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.UnsupportedEncodingException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;

public class Main {
	
	public static final String TAG = "evolve-3";

    public static void main(String[] args) throws IOException {
    	long starttime = System.currentTimeMillis();
    	Statistics stats = new Statistics();
    	//System.out.println("conway.: " + commands.Command.translate("conway.".toLowerCase()));
        Arguments arguments = processArgs(args);
        List<Solution> solutions = new ArrayList<>();
        for (String fileString : arguments.getFiles()) {
            String jsonString = new String(Files.readAllBytes(Paths.get(fileString)));
            JSONObject json = new JSONObject(jsonString);
            int id = json.getInt("id");
            for (SourceStream stream : SourceStream.getSourceStreams(json)) {
            	long start = System.currentTimeMillis();
                Boardstate board = new Boardstate(json);
                Solution solution = new TaskSolver(board, stream, arguments.getPowerWords()).solve();
                solution.id = id;
                long time = System.currentTimeMillis() - start;
                solution.seconds = (int) Math.ceil(time / 1000);
                solutions.add(solution);
            }
            if (arguments.stats) stats.add(solutions);
            if (arguments.devMode) createJsonOutput(solutions, true);
            if (arguments.devMode) solutions.clear();
        }
        if (!arguments.devMode) createJsonOutput(solutions, false);
        long duration = System.currentTimeMillis() - starttime;
        if (arguments.stats) stats.writeStatsFile("results/stats.txt", duration);
    }

    private static void createJsonOutput(List<Solution> solutions, boolean writefile) {
        JSONArray combined = new JSONArray();
        long points = 0;
        for (Solution solution : solutions) {
            JSONObject output = new JSONObject();
            output.put("problemId", solution.id);
            output.put("seed", solution.seed);
            output.put("tag", TAG);
            output.put("solution", solution.commandString);
            combined.put(output);
            points += solution.points;
        }
        String file = solutions.get(0).id + "_" + TAG;
        if (writefile) writeOutputFile(combined.toString(), file);
        if (!writefile) System.out.println(combined.toString());
    }

    private static void writeOutputFile(String result, String filename) {
        String fileName = "results/" + filename + ".json";
        PrintWriter writer;
        try {
            writer = new PrintWriter(fileName, "UTF-8");
        } catch (FileNotFoundException | UnsupportedEncodingException e) {
                throw new RuntimeException(e);
        }
        writer.println(result);
        writer.close();
    }

    private static Arguments processArgs(String[] args) {
        List<String> files = new ArrayList<String>();
        List<String> powerWords = new ArrayList<String>();
        int seconds = 0;
        int megabytes = 0;
        boolean devmode = false;
        boolean stats = false;
        int cores = 0;
        for (int i = 0; i < args.length - 1; i += 2) {
            String flag = args[i];
            String value = args[i + 1];
            switch (flag) {
                case "-c" :     cores = Integer.parseInt(value);
                                break;
                case "-dev" :   devmode = Boolean.parseBoolean(value);
                                break;
                case "-stats" : stats = Boolean.parseBoolean(value);
                                break;
                case "-f" :     files.add(value);
                                break;
                case "-t" :     seconds = Integer.parseInt(value);
                                break;
                case "-m" :     megabytes = Integer.parseInt(value);
                                break;
                case "-p" :     powerWords.add(value.replace("\"", ""));
                                break;
                case "--delay":
                    // delay for attaching profile
                    try {
                        Thread.sleep(30000);
                    } catch (InterruptedException e) {
                        // can ignore here
                    }
                    break;
                default:
                    System.err.println("warning: unknown argument " + flag);
                    break;
            }
        }
        return new Arguments(files, powerWords, seconds, megabytes, devmode, stats, cores);
    }

    private static class Arguments {
        private final List<String> files;
        private final List<String> powerWords;
        public final int secondsTimeLimit;
        public final boolean devMode;
        public final boolean stats;
        public final int cores;

        public Arguments(List<String> files, List<String> powerWords, int seconds, int megabytes, boolean devmode, boolean stats, int cores) {
            this.files = new ArrayList<String>(files);
            this.powerWords = new ArrayList<String>(powerWords);
            this.secondsTimeLimit = seconds;
            this.devMode = devmode;
            this.stats = stats;
            this.cores = cores;
        }

        public List<String> getFiles() {
            return new ArrayList<String>(files);
        }

        public List<String> getPowerWords() {
            return new ArrayList<String>(powerWords);
        }
    }
}
