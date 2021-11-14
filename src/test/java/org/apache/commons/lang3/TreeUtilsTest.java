/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.commons.lang3;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.util.*;

public class TreeUtilsTest {

    @Test
    public void testPojoBuildTee(){
        List<TreeNode> list = new ArrayList<>();
        list.add(new TreeNode(2, null, "s1"));
        list.add(new TreeNode(1, null, "s0"));
        list.add(new TreeNode(10, 1, "s10"));
        list.add(new TreeNode(311, 31, "s311"));
        list.add(new TreeNode(11, 1, "s11"));
        list.add(new TreeNode(31, 3, "s31"));
        list.add(new TreeNode(112, 11, "s112"));
        final List<TreeNode> treeList = TreeUtils.buildTree(list,
                node -> {
                    node.setTop(true);
                    node.setLevel(1);
                    node.setParentHierarchyId(String.valueOf(node.getId()));
                    node.setLeaf(false);
                },
                TreeNode::getId,
                TreeNode::getPid,
                Comparator.comparing(TreeNode::getId),
                ((node, children) -> {
                    node.setLeaf(false);
                    for (TreeNode child : children) {
                        child.setParentHierarchyId(node.getParentHierarchyId() + ">" + child.getId());
                        child.setLeaf(true);
                        child.setLevel(node.getLevel() + 1);
                        child.setTop(false);
                    }
                    node.setChildren(children);
                }));
        Assertions.assertTrue(!treeList.isEmpty() && !treeList.get(0).getChildren().isEmpty());
    }

    @Test
    public void testMapBuildTree(){
        List<Map<Object, Object>> list = new ArrayList<>();
        list.add(parseMap("100", "s100", null));
        list.add(parseMap("200", "s200", "000"));
        list.add(parseMap("110", "s110", "100"));
        list.add(parseMap("120", "s120", "100"));
        list.add(parseMap("210", "s210", "200"));
        list.add(parseMap("211", "s211", "210"));
        final List<Map<Object, Object>> treeList = TreeUtils.buildTree(list,
                (node) -> {
                    node.put("top", true);
                    node.put("level", 1);
                    node.put("parentHierarchyId", node.get("id"));
                    node.put("leaf", false);
                },
                node -> node.get("id"),
                node -> node.get("pid"),
                Comparator.comparing(node -> (String) node.get("id")),
                (node, children) -> {
                    node.put("leaf", false);
                    for (Map<Object, Object> child : children) {
                        child.put("parentHierarchyId", (String) node.get("parentHierarchyId") + node.get("id"));
                        child.put("leaf", false);
                        child.put("level", (Integer) node.get("level") + 1);
                        child.put("top", false);
                    }
                    node.put("children", children);
                });
        //noinspection unchecked
        Assertions.assertTrue(!treeList.isEmpty()
                && !((List<Map<Object, Object>>)treeList.get(0).get("children")).isEmpty());
    }

    private Map<Object, Object> parseMap(String id, String name, String pid){
        Map<Object, Object> map = new HashMap<>();
        map.put("id", id);
        map.put("name", name);
        map.put("pid", pid);
        return map;
    }

    /**
     * tree node
     * @author <a href="mailto:hjm0928@gmail.com">ejums</a>
     */
    public class TreeNode{
        // base field
        private Integer id;
        private Integer pid;
        private String name;
        // compute field
        private Integer level;
        private String parentHierarchyId;
        private Boolean top;
        private Boolean leaf;
        private List<TreeNode> children;

        public TreeNode(Integer id, Integer pid, String name){
            this.id = id;
            this.pid = pid;
            this.name = name;
        }

        public Integer getId() {
            return id;
        }

        public void setId(Integer id) {
            this.id = id;
        }

        public Integer getPid() {
            return pid;
        }

        public void setPid(Integer pid) {
            this.pid = pid;
        }

        public String getName() {
            return name;
        }

        public void setName(String name) {
            this.name = name;
        }

        public Integer getLevel() {
            return level;
        }

        public void setLevel(Integer level) {
            this.level = level;
        }

        public String getParentHierarchyId() {
            return parentHierarchyId;
        }

        public void setParentHierarchyId(String parentHierarchyId) {
            this.parentHierarchyId = parentHierarchyId;
        }

        public Boolean getTop() {
            return top;
        }

        public void setTop(Boolean top) {
            this.top = top;
        }

        public Boolean getLeaf() {
            return leaf;
        }

        public void setLeaf(Boolean leaf) {
            this.leaf = leaf;
        }

        public List<TreeNode> getChildren() {
            return children;
        }

        public void setChildren(List<TreeNode> children) {
            this.children = children;
        }
    }
}
