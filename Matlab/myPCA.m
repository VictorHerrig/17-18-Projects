function [pc, ev] = myPCA(data)
    [vectors, values] = eig(cov(data));
    [ev, index] = sort(values(values > 0), 'descend');
    pc = vectors(:, index);
end