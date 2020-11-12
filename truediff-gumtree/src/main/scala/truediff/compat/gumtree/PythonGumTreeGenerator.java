/*
 * This file was part of GumTree. It was copied and adapted for use with truediff.
 *
 * GumTree is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * GumTree is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with GumTree.  If not, see <http://www.gnu.org/licenses/>.
 *
 * Copyright 2016 Jean-Rémy Falleri <jr.falleri@gmail.com>
 */

package truediff.compat.gumtree;

import com.github.gumtreediff.gen.ExternalProcessTreeGenerator;
import com.github.gumtreediff.gen.Register;
import com.github.gumtreediff.gen.Registry;
import com.github.gumtreediff.io.LineReader;
import com.github.gumtreediff.tree.ITree;
import com.github.gumtreediff.tree.TreeContext;

import javax.xml.namespace.QName;
import javax.xml.stream.XMLEventReader;
import javax.xml.stream.XMLInputFactory;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.events.StartElement;
import javax.xml.stream.events.XMLEvent;
import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import java.util.ArrayDeque;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@Register(id = "python-pythonparser", accept = {"\\.py$"}, priority = Registry.Priority.MAXIMUM)
public class PythonGumTreeGenerator extends ExternalProcessTreeGenerator {

    private static final String PYTHONPARSER_CMD = System.getProperty("gt.pp.path",
            "/Users/seba/projects/external/pythonparser/src/main/python/pythonparser/pythonparser_3.py");

    private static final QName VALUE = new QName("value");

    private static final QName LINENO = new QName("lineno");

    private static final QName COL = new QName("col");

    private static final QName END_LINENO = new QName("end_line_no");

    private static final QName END_COL = new QName("end_col");

    private LineReader lr;

    private DiffableGumTreeContext context;

    public String generateXml(String input) throws IOException {
        return readStandardOutput(new StringReader(input));
    }

    @Override
    public DiffableGumTreeContext generate(Reader r) throws IOException {
        lr = new LineReader(r);
        String output = readStandardOutput(lr);
        return getTreeContext(output);
    }

    public DiffableGumTreeContext generateFromXml(String xml, String[] lines) {
        lr = new LineReader(null) {
            @Override
            public int positionFor(int line, int column) {
                return lines == null ? -1 : lines[line - 1].length() + column - 1;
            }
        };
        return getTreeContext(xml);
    }

    private DiffableGumTreeContext getTreeContext(String xml) {
        XMLInputFactory fact = XMLInputFactory.newInstance();
        context = new DiffableGumTreeContext();
        try {
            ArrayDeque<ITree> trees = new ArrayDeque<>();
            xml = xml.replace('\u0008', '@').replace('\u001b', '@');
            XMLEventReader r = fact.createXMLEventReader(new StringReader(xml));
            while (r.hasNext()) {
                XMLEvent ev = r.nextEvent();
                if (ev.isStartElement()) {
                    StartElement s = ev.asStartElement();
                    String typeLabel = s.getName().getLocalPart();
                    String label = "";
                    if (s.getAttributeByName(VALUE) != null)
                        label = s.getAttributeByName(VALUE).getValue();
                    int type = typeLabel.hashCode();
                    ITree t = context.createTree(type, label, typeLabel);
                    if (trees.isEmpty()) {
                        context.setRoot(t);
                    } else {
                        t.setParentAndUpdateChildren(trees.peekFirst());
                    }
                    setPos(t, s);
                    trees.addFirst(t);
                } else if (ev.isEndElement())
                    trees.removeFirst();
            }
            context.validate();
            return context;
        } catch (XMLStreamException e) {
            throw new RuntimeException(e);
        }
    }

    private void setPos(ITree t, StartElement e) {
        if (e.getAttributeByName(LINENO) == null) { //FIXME some nodes have start position
            System.out.println(t.getLabel());
            return;
        }
        int line = Integer.parseInt(e.getAttributeByName(LINENO).getValue());
        int column = Integer.parseInt(e.getAttributeByName(COL).getValue());
        t.setPos(lr.positionFor(line, column) + 2);
        if (e.getAttributeByName(END_LINENO) == null) { //FIXME some nodes have no end position
            System.out.println(t.getLabel());
            return;
        }
        int endLine = Integer.parseInt(e.getAttributeByName(END_LINENO).getValue());
        int endColumn = Integer.parseInt(e.getAttributeByName(END_COL).getValue());
        t.setLength(lr.positionFor(endLine, endColumn) - lr.positionFor(line, column));
    }

    public String[] getCommandLine(String file) {
        return new String[]{PYTHONPARSER_CMD, file};
    }
}
