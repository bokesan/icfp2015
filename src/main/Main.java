package main;


import org.json.JSONArray;
import org.json.JSONObject;

import commands.Command;
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
	
	private static final String TAG = "width-bug-fixed";

    public static void main(String[] args) throws IOException {
    	//System.out.println(Command.translate("Ph'nglui mglw'nafh Cthulhu R'lyeh wgah'nagl fhtagn.".toLowerCase()));
        Arguments arguments = processArgs(args);
        List<Solution> solutions = new ArrayList<>();
        for (String fileString : arguments.getFiles()) {
            String jsonString = new String(Files.readAllBytes(Paths.get(fileString)));
            JSONObject json = new JSONObject(jsonString);
            int id = json.getInt("id");
            for (SourceStream stream : SourceStream.getSourceStreams(json)) {
                Boardstate board = new Boardstate(json);
                Solution solution = new TaskSolver(board, stream, arguments.getPowerWords()).solve();
                solution.id = id;
                solutions.add(solution);
            }
            createJsonOutput(solutions);
            solutions.clear();
        }
        //createJsonOutput(solutions);
    }

    private static void createJsonOutput(List<Solution> solutions) {
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
        int average = (int) Math.floor(points / solutions.size());
        String file = solutions.get(0).id + "_" + solutions.size() + "_" + average + "_" + TAG;
        writeOutputFile(combined.toString(), file);
        //System.out.println(combined.toString());
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
        for (int i = 0; i < args.length - 1; i += 2) {
            String flag = args[i];
            String value = args[i + 1];
            switch (flag) {
                case "-f" : files.add(value);
                            break;
                case "-t" : seconds = Integer.parseInt(value);
                            break;
                case "-m" : megabytes = Integer.parseInt(value);
                            break;
                case "-p" : powerWords.add(value.replace("\"", ""));
                            break;
                default:    //nothing, ignore;
            }
        }
        return new Arguments(files, powerWords, seconds, megabytes);
    }

    private static class Arguments {
        private final List<String> files;
        private final List<String> powerWords;
        public final int secondsTimeLimit;
        public final int megabyteRamLimit;

        public Arguments(List<String> files, List<String> powerWords, int seconds, int megabytes) {
            this.files = new ArrayList<String>(files);
            this.powerWords = new ArrayList<String>(powerWords);
            this.secondsTimeLimit = seconds;
            this.megabyteRamLimit = megabytes;
        }

        public List<String> getFiles() {
            return new ArrayList<String>(files);
        }

        public List<String> getPowerWords() {
            return new ArrayList<String>(powerWords);
        }
    }
}
