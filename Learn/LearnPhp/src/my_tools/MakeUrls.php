<?php

class MakeUrls {
    private $_source;
    private $_to_close;

    public function __construct($source) {
        $this->_source = $source;
        $this->_to_close = array();
    }

    function __destruct () {
        $this->close_files();
    }

    public function process() {
        $in = $this->open_source_file();
        $out = $this->open_dest_file();

        $this->make_prolog($out);
        $this->make_body($in, $out);
        $this->make_epilogue($out);

        $this->close_files();
    }

    private function make_body($in, $out)
    {
        $line = fgets($in);
        while ($line) {
            fputs($out, sprintf("<a href=\"%s\">%s</a><br/>\n", $line, $line));
            $line = fgets($in);
        }
    }

    private function make_prolog($out) {
        fputs($out, "<html>");
        fputs($out, "<head>");
        fputs($out, "<title>URLs</title>");
        fputs($out, "</head>");
        fputs($out, "<body>");
    }

    private function make_epilogue($out) {
        fputs($out, "</body>");
        fputs($out, "</html>");
    }

    private function open_source_file()
    {
        $in = fopen($this->_source, "r");
        if (!$in)
            throw new Exception("Did not find $this->_source file.");
        $this->_to_close[] = $in;
        return $in;
    }

    private function open_dest_file()
    {
        $index = strrpos($this->_source, ".");
        if ($index == 0)
            throw new Exception("The specified source file name: $this->_source does not have an extension.");

        $dest = substr($this->_source, 0, $index + 1) . "html";
        $out = fopen($dest, "w");
        $this->_to_close[] = $out;

        return $out;
    }

    private function close_files() {
        foreach ($this->_to_close as $h)
            fclose($h);
        $this->_to_close = array();
    }
}

try {
    $processor = new MakeUrls("d:/a1.txt");
    $processor->process();
}
catch (Exception $e) {
    printf("There is exception:  %s \n", $e->getMessage());
}

?>
